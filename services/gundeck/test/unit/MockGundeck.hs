{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module MockGundeck where

import Imports
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.State
import Data.Id
import Data.List1                     (List1)
import Data.Misc ((<$$>))
import Gundeck.Aws.Arn as Aws
import Gundeck.Options
import Gundeck.Push
import Gundeck.Push.Native              as Native
import Gundeck.Types
import System.Random
import Test.QuickCheck as QC
import Test.QuickCheck.Instances ()

import qualified Data.Aeson.Types       as Aeson
import qualified Data.Set as Set
import qualified Network.URI as URI


----------------------------------------------------------------------
-- env

data MockEnv = MockEnv
  { _meStdGen          :: StdGen
  , _mePresences       :: [(UserId, [Presence])]
  , _meNativeAddress   :: Presence -> Maybe (Address "no-keys")
  , _meWSReachable     :: Presence -> Bool
  , _meNativeReachable :: Address "no-keys" -> Bool
  , _meWSQueue         :: Set NotificationId
  , _meNativeQueue     :: Set NotificationId
  }

genMockEnv :: Gen MockEnv
genMockEnv = do
  _meStdGen <- do
    mkStdGen <$> arbitrary

  _mePresences <- do
    uids :: [UserId] <- arbitrary
    forM uids $ \uid -> (uid,) <$> listOf1 (genPresence uid)

  prcs :: [Presence]
    <- shuffle . mconcat $ snd <$> _mePresences
  protoaddrs :: [UserId -> ClientId -> ConnId -> Address "no-keys"]
    <- do
      len <- choose (length prcs `div` 2, length prcs)
      vectorOf len genProtoAddress

  let addrs :: [(Presence, Address "no-keys")]
      addrs = zipWith go prcs protoaddrs
        where go prc adr = (prc, adr (userId prc) (fromJust $ clientId prc) (connId prc))

      _meNativeAddress :: Presence -> Maybe (Address "no-keys")
      _meNativeAddress = (`lookup` addrs)

  _meWSReachable <- genPredicate prcs
  _meNativeReachable <- genPredicate (snd <$> addrs)

  let _meWSQueue = mempty
      _meNativeQueue = mempty

  pure MockEnv {..}

genPredicate :: forall a. (Eq a) => [a] -> Gen (a -> Bool)
genPredicate xs = do
  bools :: [Bool] <- vectorOf (length xs) arbitrary
  let reachables :: [a] = mconcat $ zipWith (\x yes -> [ x | yes ]) xs bools
  pure (`elem` reachables)

genPresence :: HasCallStack => UserId -> Gen Presence
genPresence uid = do
  connId   <- ConnId <$> arbitrary
  clientId <- Just . newClientId <$> arbitrary
  let userId    = uid
      resource  = URI . fromJust $ URI.parseURI "http://example.com"
      createdAt = 0
      __field   = mempty
  pure Presence {..}

genProtoAddress :: Gen (UserId -> ClientId -> ConnId -> Address "no-keys")
genProtoAddress = do
  _addrTransport <- QC.elements [minBound..]
  arnEpId <- EndpointId <$> arbitrary
  let _addrApp = "AppName"
      _addrToken = Token "tok"
      _addrEndpoint = Aws.mkSnsArn Tokyo (Account "acc") eptopic
      eptopic = mkEndpointTopic (ArnEnv "") _addrTransport _addrApp arnEpId
  pure $ \_addrUser _addrClient _addrConn -> Address {..}

genPushes :: MockEnv -> Gen [Push]
genPushes _env = undefined


----------------------------------------------------------------------
-- monad type and instances

newtype MockGundeck a = MockGundeck { fromMockGundeck :: StateT MockEnv (ExceptT String Identity) a }
  deriving (Functor, Applicative, Monad, MonadState MockEnv, MonadError String)

makeLenses 'MockEnv

instance MonadThrow MockGundeck where
  throwM = error . show  -- (this is a bit lazy, yes.)

instance MonadPushAll MockGundeck where
  mpaNotificationTTL = pure $ NotificationTTL 300  -- (longer than we want any test to take.)
  mpaMkNotificationId = mockMkNotificationId
  mpaListAllPresences = mockListAllPresences
  mpaBulkPush = mockBulkPush
  mpaStreamAdd = mockStreamAdd
  mpaPushNative = mockPushNative
  mpaForkIO = id  -- just don't fork.  (this *may* cause deadlocks in principle, but as long as it
                  -- doesn't, this is good enough for testing).

instance MonadNativeTarget MockGundeck where
  mntgtLogErr _ = pure ()
  mntgtLookupAddress = mockLookupAddress
  mntgtMapAsync f xs = Right <$$> mapM f xs  -- (no concurrency)


----------------------------------------------------------------------
-- monad implementation

-- | (There is certainly a fancier implementation using '<%=' or similar, but this one is easier to
-- reverse engineer later.)
mockMkNotificationId
  :: (HasCallStack, m ~ MockGundeck)
  => m NotificationId
mockMkNotificationId = Id <$> state go
  where
    go env = case random (env ^. meStdGen) of
      (uuid, g') -> (uuid, env & meStdGen .~ g')

mockListAllPresences
  :: (HasCallStack, m ~ MockGundeck)
  => [UserId] -> m [[Presence]]
mockListAllPresences uids = do
  hits :: [(UserId, [Presence])] <- filter ((`elem` uids) . fst) <$> gets (^. mePresences)
  pure $ snd <$> hits

mockBulkPush
  :: (HasCallStack, m ~ MockGundeck)
  => [(Notification, [Presence])] -> m [(NotificationId, [Presence])]
mockBulkPush notifs = do
  isreachchable <- gets (^. meWSReachable)
  good :: [Presence] <- filter isreachchable . mconcat <$> (snd <$$> gets (^. mePresences))
  pure [ (nid, prcs)
       | (ntfId -> nid, filter (`elem` good) -> prcs) <- notifs
       , not $ null prcs
       ]

mockStreamAdd
  :: (HasCallStack, m ~ MockGundeck)
  => NotificationId -> List1 NotificationTarget -> List1 Aeson.Object -> NotificationTTL -> m ()
mockStreamAdd nid _ _ _ = modify $ meWSQueue %~ Set.insert nid

mockPushNative
  :: (HasCallStack, m ~ MockGundeck)
  => Notification -> Push -> [Address "no-keys"] -> m ()
mockPushNative (ntfId -> nid) _ addrs = do
  isreachable <- gets (^. meNativeReachable)
  forM_ addrs $ \addr -> do
    when (isreachable addr) . modify $ meNativeQueue %~ Set.insert nid

mockLookupAddress
  :: (HasCallStack, m ~ MockGundeck)
  => UserId -> m [Address "no-keys"]
mockLookupAddress uid = do
  getaddr <- gets (^. meNativeAddress)
  users :: [(UserId, [Presence])] <- gets (^. mePresences)
  mockprcs :: [Presence] <- maybe (error "user not found!") pure $ lookup uid users
  pure . catMaybes $ getaddr <$> mockprcs
