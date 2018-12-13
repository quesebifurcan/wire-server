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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-orphans #-}

module MockGundeck where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.State
import Data.Id
import Data.List1
import Data.List1 (List1)
import Data.Misc ((<$$>))
import Data.Range
import Gundeck.Aws.Arn as Aws
import Gundeck.Options
import Gundeck.Push
import Gundeck.Push.Native as Native
import Gundeck.Types
import Imports
import System.Random
import Test.QuickCheck as QC
import Test.QuickCheck.Instances ()

import qualified Data.Aeson.Types as Aeson
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Network.URI as URI


----------------------------------------------------------------------
-- env

data MockEnv = MockEnv
  { _meStdGen          :: StdGen
  , _mePresences       :: [(UserId, [Presence])]
  , _meNativeAddress   :: [(Presence, Address "no-keys")]
  , _meWSReachable     :: [Presence]
  , _meNativeReachable :: [Address "no-keys"]
  , _meWSQueue         :: Set NotificationId
  , _meNativeQueue     :: Set NotificationId
  }
  deriving (Show)

-- | Generate an environment probabilistically containing the following situations:
--
-- 1. device connected to web socket, web socket delivery will work
-- 2. device connected to web socket, web socket delivery will NOT work
-- 3. device NOT connected to web socket, native push token registered, push will succeed
-- 4. device NOT connected to web socket, native push token registered, push will fail
-- 5. device NOT connected to web socket, no native push token registered
--
-- TODO: trying to undrestand gundeck...
--
-- There is ConnId and ClientId.  The comments that I wrote (and @Tiago approved) say that Connid is
-- a temporary handle for the web socket connection, and ClientId is the fingerprint (kinda).  But
-- the Presence type and the persistence functions in Gundeck.Push.Data suggest that ConnId is
-- always there, and ClientId only sometimes.  how does that fit?
--
-- also, i need to match a ClientId to a push token, but it looks like there is only a mapping from
-- UserId to all Addresses of the user (pointing to all devices via push, and containing a Token,
-- but not a PushToken.
--
-- i get a feeling that there is a lot more room for simplifying things...
--
-- some wild guesses: (1) there is a mapping UserId, ClientId -> Address in Gundeck.Push.Data,
-- simplified to UserId -> [Address] (where Address contains the ClientId).  (2) the ConnId in rows
-- where there is no websocket is just outdated or empty or something, and will be ignored.  (3) the
-- fact that ClientId is allowed to be Maybe in Presence has historical reasons and is not used any
-- more.  (4) there is always a Presence for a device, even if there never was a websocket
-- connection.
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

  let _meNativeAddress :: [(Presence, Address "no-keys")]
      _meNativeAddress = zipWith go prcs protoaddrs
        where go prc adr = (prc, adr (userId prc) (fromJust $ clientId prc) (connId prc))

  _meWSReachable <- genPredicate prcs
  _meNativeReachable <- genPredicate (snd <$> _meNativeAddress)

  let _meWSQueue = mempty
      _meNativeQueue = mempty

  pure MockEnv {..}

genPredicate :: forall a. (Eq a) => [a] -> Gen [a]
genPredicate xs = do
  bools :: [Bool] <- vectorOf (length xs) arbitrary
  let reachables :: [a] = mconcat $ zipWith (\x yes -> [ x | yes ]) xs bools
  pure reachables

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

genPushes :: [(UserId, [Presence])] -> Gen [Push]
genPushes = listOf . genPush

-- | REFACTOR: What 'Route's are we still using, and what for?  This code is currently only testing
-- 'RouteAny'.
genPush :: [(UserId, [Presence])] -> Gen Push
genPush env = do
  uid :: UserId <- fst <$> QC.elements env
  rcps :: Range 1 1024 (Set Recipient) <- do
    numrcp <- choose (1, 1024)
    usrs <- vectorOf numrcp (QC.elements env)
    let mkrcp (u, ps) = Recipient u RouteAny (catMaybes $ clientId <$> ps)
    pure . unsafeRange . Set.fromList $ mkrcp <$> usrs
  pload <- List1 <$> arbitrary
  pure $ newPush uid rcps pload

instance Arbitrary Aeson.Value where
  arbitrary = oneof
    [ Aeson.object <$> listOf ((Aeson..=) <$> arbitrary <*> (scale (`div` 3) (arbitrary @Aeson.Value)))
    , Aeson.Array . Vector.fromList <$> listOf (scale (`div` 3) (arbitrary @Aeson.Value))
    , Aeson.String <$> arbitrary
    , Aeson.Number <$> arbitrary
    , Aeson.Bool <$> QC.elements [minBound..]
    , pure Aeson.Null
    ]


----------------------------------------------------------------------
-- monad type and instances

newtype MockGundeck a = MockGundeck { fromMockGundeck :: StateT MockEnv Identity a }
  deriving (Functor, Applicative, Monad, MonadState MockEnv)

makeLenses 'MockEnv

runMockGundeck :: MockEnv -> MockGundeck a -> (a, MockEnv)
runMockGundeck env (MockGundeck m) = runIdentity $ runStateT m env

instance MonadThrow MockGundeck where
  throwM = error . show  -- (this is the bad kind of lazy, yes.)

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
  (flip elem -> isreachchable) <- gets (^. meWSReachable)
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
  (flip elem -> isreachable) <- gets (^. meNativeReachable)
  forM_ addrs $ \addr -> do
    when (isreachable addr) . modify $ meNativeQueue %~ Set.insert nid

mockLookupAddress
  :: (HasCallStack, m ~ MockGundeck)
  => UserId -> m [Address "no-keys"]
mockLookupAddress uid = do
  (flip lookup -> getaddr) <- gets (^. meNativeAddress)
  users :: [(UserId, [Presence])] <- gets (^. mePresences)
  mockprcs :: [Presence] <- maybe (error "user not found!") pure $ lookup uid users
  pure . catMaybes $ getaddr <$> mockprcs
