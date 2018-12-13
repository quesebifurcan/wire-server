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
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Network.URI as URI


----------------------------------------------------------------------
-- env

type Payload = List1 Aeson.Object

data MockEnv = MockEnv
  { _meStdGen          :: StdGen
  , _meRecipients      :: [Recipient]
  , _meNativeAddress   :: Map Recipient (Address "no-keys")
  , _meWSReachable     :: Set Recipient
  , _meNativeReachable :: Set (Address "no-keys")
  , _meNativeQueue     :: Map (UserId, ClientId) Payload
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

  _meRecipients :: [Recipient]
    <- listOf1 genRecipient

  protoaddrs :: [UserId -> ClientId -> Address "no-keys"]
    <- do
      len <- let l = length _meRecipients in choose (l `div` 2, l)
      vectorOf len genProtoAddress

  let _meNativeAddress :: Map Recipient (Address "no-keys")
      _meNativeAddress = Map.fromList addrs

      addrs :: [(Recipient, Address "no-keys")]
      addrs = mconcat $ zipWith go _meRecipients protoaddrs
        where go rcp@(Recipient uid _ cids) adr = (\cid -> (rcp, adr uid cid)) <$> cids

  _meWSReachable <- genPredicate _meRecipients
  _meNativeReachable <- genPredicate (snd <$> addrs)

  let _meNativeQueue = mempty

  pure MockEnv {..}

genPredicate :: forall a. (Eq a, Ord a) => [a] -> Gen (Set a)
genPredicate xs = Set.fromList <$> do
  bools :: [Bool] <- vectorOf (length xs) arbitrary
  let reachables :: [a] = mconcat $ zipWith (\x yes -> [ x | yes ]) xs bools
  pure reachables

genRecipient :: HasCallStack => Gen Recipient
genRecipient = do
  uid  <- arbitrary
  cids <- newClientId <$$> arbitrary
  pure $ Recipient uid RouteAny cids

fakePresences :: Recipient -> [Presence]
fakePresences (Recipient uid _ cids) = fakePresence uid <$> cids

fakePresence :: UserId -> ClientId -> Presence
fakePresence userId (Just -> clientId) = Presence {..}
  where
    connId    = fakeConnId
    resource  = URI . fromJust $ URI.parseURI "http://example.com"
    createdAt = 0
    __field   = mempty

fakeConnId :: ConnId
fakeConnId = ConnId mempty

genProtoAddress :: Gen (UserId -> ClientId -> Address "no-keys")
genProtoAddress = do
  _addrTransport <- QC.elements [minBound..]
  arnEpId <- EndpointId <$> arbitrary
  let _addrApp = "AppName"
      _addrToken = Token "tok"
      _addrEndpoint = Aws.mkSnsArn Tokyo (Account "acc") eptopic
      eptopic = mkEndpointTopic (ArnEnv "") _addrTransport _addrApp arnEpId
      _addrConn = fakeConnId
  pure $ \_addrUser _addrClient -> Address {..}

genPushes :: [Recipient] -> Gen [Push]
genPushes = listOf . genPush

-- | REFACTOR: What 'Route's are we still using, and what for?  This code is currently only testing
-- 'RouteAny'.
genPush :: [Recipient] -> Gen Push
genPush allrcps = do
  sender :: UserId <- (^. recipientId) <$> QC.elements allrcps
  rcps :: Range 1 1024 (Set Recipient) <- do
    numrcp <- choose (1, min 1024 (length allrcps))
    unsafeRange . Set.fromList <$> vectorOf numrcp (QC.elements allrcps)
  pload <- List1 <$> arbitrary
  pure $ newPush sender rcps pload

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
  hits :: [Recipient] <- filter ((`elem` uids) . (^. recipientId)) <$> gets (^. meRecipients)
  pure $ fakePresences <$> hits

mockBulkPush
  :: (HasCallStack, m ~ MockGundeck)
  => [(Notification, [Presence])] -> m [(NotificationId, [Presence])]
mockBulkPush notifs = do
  (flip elem -> isreachchable) <- gets (^. meWSReachable)
  good :: [Presence]
    <- mconcat . fmap fakePresences . filter isreachchable <$> gets (^. meRecipients)
  pure [ (nid, prcs)
       | (ntfId -> nid, filter (`elem` good) -> prcs) <- notifs
       , not $ null prcs
       ]

-- | persisting notification is not needed for the tests at the moment, so we do nothing here.
mockStreamAdd
  :: (HasCallStack, m ~ MockGundeck)
  => NotificationId -> List1 NotificationTarget -> Payload -> NotificationTTL -> m ()
mockStreamAdd _ _ _ _ = pure ()

mockPushNative
  :: (HasCallStack, m ~ MockGundeck)
  => Notification -> Push -> [Address "no-keys"] -> m ()
mockPushNative _nid ((^. pushPayload) -> payload) addrs = do
  (flip elem -> isreachable) <- gets (^. meNativeReachable)
  forM_ addrs $ \addr -> do
    when (isreachable addr) . modify $ meNativeQueue %~ Map.insert (addr ^. addrUser, addr ^. addrClient) payload

mockLookupAddress
  :: (HasCallStack, m ~ MockGundeck)
  => UserId -> m [Address "no-keys"]
mockLookupAddress uid = do
  (flip Map.lookup -> getaddr) <- gets (^. meNativeAddress)
  rcps :: [Recipient] <- filter ((== uid) . (^. recipientId)) <$> gets (^. meRecipients)
  pure . catMaybes $ getaddr <$> rcps
