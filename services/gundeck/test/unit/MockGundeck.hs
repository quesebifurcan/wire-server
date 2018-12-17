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

import Imports
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.State
import Data.Id
import Data.List1
import Data.Misc ((<$$>), Milliseconds(Ms))
import Data.Range
import Data.String.Conversions
import Data.UUID ()
import Gundeck.Aws.Arn as Aws
import Gundeck.Options
import Gundeck.Push
import Gundeck.Push.Native as Native
import Gundeck.Push.Websocket as Web
import Gundeck.Types
import Gundeck.Types.BulkPush
import System.Random
import System.Logger.Class as Log
import Test.QuickCheck as QC
import Test.QuickCheck.Instances ()
import Text.Show.Pretty (ppShow)

import qualified Data.Aeson.Types as Aeson
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Network.URI as URI


----------------------------------------------------------------------
-- helpers (move to some more general-purpose test library?)

newtype Pretty a = Pretty a
  deriving (Eq, Ord)

instance Show a => Show (Pretty a) where
  show (Pretty a) = ppShow a


----------------------------------------------------------------------
-- env

type Payload = List1 Aeson.Object

data MockEnv = MockEnv
  { _meStdGen          :: StdGen
  , _meRecipients      :: [Recipient]
  , _meNativeAddress   :: Map UserId (Map ClientId (Address "no-keys"))
  , _meWSReachable     :: Set (UserId, ClientId)
  , _meNativeReachable :: Set (Address "no-keys")
    -- TODO: the above fields will not change in MockGundeck, so they could be moved to a
    -- ReadOnlyMockEnv that is stored in a Reader monad.
  , _meNativeQueue     :: Map (UserId, ClientId) Payload
  }
  deriving (Show)

makeLenses ''MockEnv

-- | Generate an environment probabilistically containing the following situations:
--
-- 1. web socket delivery will work
-- 2. web socket delivery will NOT work, native push token registered, push will succeed
-- 4. web socket delivery will NOT work, native push token registered, push will fail
-- 5. web socket delivery will NOT work, no native push token registered
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

  let addrs :: [(Recipient, Address "no-keys")]
      addrs = mconcat $ zipWith go _meRecipients protoaddrs
        where go rcp@(Recipient uid _ cids) adr = (\cid -> (rcp, adr uid cid)) <$> cids

      _meNativeAddress :: Map UserId (Map ClientId (Address "no-keys"))
      _meNativeAddress = foldl' go mempty (snd <$> addrs)
        where
          go :: Map UserId (Map ClientId (Address "no-keys"))
             -> Address "no-keys"
             -> Map UserId (Map ClientId (Address "no-keys"))
          go m addr = Map.alter (go' addr) (addr ^. addrUser) m

          go' :: Address "no-keys"
              -> Maybe (Map ClientId (Address "no-keys"))
              -> Maybe (Map ClientId (Address "no-keys"))
          go' addr = Just . maybe new (new <>)
            where new = Map.fromList [(addr ^. addrClient, addr)]

  _meWSReachable <- genPredicate . mconcat $ recipientToIds <$> _meRecipients
  _meNativeReachable <- genPredicate (snd <$> addrs)

  let _meNativeQueue = mempty
      env = MockEnv {..}

  validateMockEnv env & either error (const $ pure env)

validateMockEnv :: MockEnv -> Either String ()
validateMockEnv env = do
  forM_ (Map.toList $ env ^. meNativeAddress) $ \(uid, el) -> do
    forM_ (Map.toList el) $ \(cid, adr) -> do
      unless (uid == adr ^. addrUser && cid == adr ^. addrClient) $ do
        throwError (show (uid, cid, adr))

recipientToIds :: Recipient -> [(UserId, ClientId)]
recipientToIds (Recipient uid _ cids) = (uid,) <$> cids

genPredicate :: forall a. (Eq a, Ord a) => [a] -> Gen (Set a)
genPredicate xs = Set.fromList <$> do
  bools :: [Bool] <- vectorOf (length xs) arbitrary
  let reachables :: [a] = mconcat $ zipWith (\x yes -> [ x | yes ]) xs bools
  pure reachables

genRecipient :: HasCallStack => Gen Recipient
genRecipient = do
  uid  <- genUserId
  cids <- listOf1 genClientId
  pure $ Recipient uid RouteAny cids

genUserId :: Gen UserId
genUserId = do
  gen <- mkStdGen <$> resize 100 arbitrary
  pure . Id . fst $ random gen

genClientId :: Gen ClientId
genClientId = newClientId <$> resize 50 arbitrary

fakePresences :: Recipient -> [Presence]
fakePresences (Recipient uid _ cids) = fakePresence uid <$> cids

fakePresence :: UserId -> ClientId -> Presence
fakePresence userId (Just -> clientId) = Presence {..}
  where
    connId    = fakeConnId $ fromJust clientId
    resource  = URI . fromJust $ URI.parseURI "http://example.com"
    createdAt = 0
    __field   = mempty

fakeConnId :: ClientId -> ConnId
fakeConnId = ConnId . cs . client

clientIdFromConnId :: ConnId -> ClientId
clientIdFromConnId = ClientId . cs . fromConnId

genProtoAddress :: Gen (UserId -> ClientId -> Address "no-keys")
genProtoAddress = do
  _addrTransport <- QC.elements [minBound..]
  arnEpId <- EndpointId <$> arbitrary
  let _addrApp = "AppName"
      _addrToken = Token "tok"
      _addrEndpoint = Aws.mkSnsArn Tokyo (Account "acc") eptopic
      eptopic = mkEndpointTopic (ArnEnv "") _addrTransport _addrApp arnEpId
  pure $ \_addrUser _addrClient -> let _addrConn = fakeConnId _addrClient in Address {..}

shrinkPretty :: (a -> [a]) -> Pretty a -> [Pretty a]
shrinkPretty shrnk (Pretty xs) = Pretty <$> shrnk xs

shrinkPushes :: [Push] -> [[Push]]
shrinkPushes = shrinkList shrinkPush
  where
    shrinkPush :: Push -> [Push]
    shrinkPush psh = (\rcps -> psh & pushRecipients .~ rcps) <$> shrinkRecipients (psh ^. pushRecipients)

    shrinkRecipients :: Range 1 1024 (Set Recipient) -> [Range 1 1024 (Set Recipient)]
    shrinkRecipients = fmap unsafeRange . map Set.fromList . filter (not . null) . shrinkList shrinkRecipient . Set.toList . fromRange

    shrinkRecipient :: Recipient -> [Recipient]
    shrinkRecipient _ = []  --  (Recipient uid route cids) = Recipient uid route <$> shrinkList shrinkNothing cids

genPushes :: [Recipient] -> Gen [Push]
genPushes = listOf . genPush

-- | REFACTOR: What 'Route's are we still using, and what for?  This code is currently only testing
-- 'RouteAny'.
genPush :: [Recipient] -> Gen Push
genPush allrcps = do
  sender :: UserId <- (^. recipientId) <$> QC.elements allrcps
  rcps :: Range 1 1024 (Set Recipient) <- do
    numrcp <- choose (1, min 1024 (length allrcps))
    unsafeRange . Set.fromList . nubBy ((==) `on` (^. recipientId)) <$> vectorOf numrcp (QC.elements allrcps)
                  -- TODO: sometimes we only want to send to some clients?  currently we always send to all explicitly
  pload <- genPayload
  pure $ newPush sender rcps pload

genPayload :: Gen Payload
genPayload = List1 <$> arbitrary

instance Arbitrary Aeson.Value where
  arbitrary = oneof
    [ Aeson.object <$> listOf ((Aeson..=) <$> arbitrary <*> (scale (`div` 3) (arbitrary @Aeson.Value)))
    , Aeson.Array . Vector.fromList <$> listOf (scale (`div` 3) (arbitrary @Aeson.Value))
    , Aeson.String <$> arbitrary
    , Aeson.Number <$> arbitrary
    , Aeson.Bool <$> QC.elements [minBound..]
    , pure Aeson.Null
    ]

genNotif :: Gen Notification
genNotif = Notification <$> arbitrary <*> arbitrary <*> genPayload

shrinkNotifs :: [(Notification, [Presence])] -> [[(Notification, [Presence])]]
shrinkNotifs = shrinkList (\(notif, prcs) -> (notif,) <$> shrinkList (const []) prcs)

genNotifs :: [Recipient] -> Gen [(Notification, [Presence])]
genNotifs (mconcat . fmap fakePresences -> allprcs) = listOf $ do
  notif <- genNotif
  prcs <- listOf $ QC.elements allprcs
  pure (notif, prcs)


----------------------------------------------------------------------
-- monad type and instances

newtype MockGundeck a = MockGundeck { fromMockGundeck :: StateT MockEnv Identity a }
  deriving (Functor, Applicative, Monad, MonadState MockEnv)

runMockGundeck :: MockEnv -> MockGundeck a -> (a, MockEnv)
runMockGundeck env (MockGundeck m) = runIdentity $ runStateT m env

instance MonadThrow MockGundeck where
  throwM = error . show  -- (we are not expecting any interesting errors in these tests, so we might
                         -- as well crash badly here, as long as it doesn't go unnoticed...)

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

instance MonadBulkPush MockGundeck where
  mbpBulkSend = mockBulkSend
  mbpDeleteAllPresences _ = pure ()  -- TODO: test presence deletion logic
  mbpPosixTime = pure $ Ms 1545045904275  -- (time is constant)
  mbpMapConcurrently = mapM  -- (no concurrency)
  mbpMonitorBadCannons _ = pure ()  -- (no monitoring)

instance Log.MonadLogger MockGundeck where
  log _ _ = pure ()  -- (no logging)


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

-- fake implementation of 'Web.bulkPush'
mockBulkPush
  :: (HasCallStack, m ~ MockGundeck)
  => [(Notification, [Presence])] -> m [(NotificationId, [Presence])]
mockBulkPush notifs = do
  env <- get

  let isreachable :: Presence -> Bool
      isreachable prc = (userId prc, fromJust $ clientId prc) `elem` (env ^. meWSReachable)

      delivered :: [Presence]
      delivered = filter isreachable . mconcat . fmap fakePresences $ env ^. meRecipients

  pure [ (nid, prcs)
       | (ntfId -> nid, filter (`elem` delivered) -> prcs) <- notifs
       , not $ null prcs  -- (sic!) (this is what gundeck currently does)
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
  getaddr <- gets (^. meNativeAddress)
  maybe (pure []) (pure . Map.elems) $ Map.lookup uid getaddr

mockBulkSend
  :: (HasCallStack, m ~ MockGundeck)
  => URI -> BulkPushRequest
  -> m (URI, Either SomeException BulkPushResponse)
mockBulkSend uri (flattenBulkPushRequest -> notifs) = do
  reachables :: Set PushTarget
    <- Set.map (uncurry PushTarget . (_2 %~ fakeConnId)) <$> gets (^. meWSReachable)
  let getstatus trgt = if trgt `Set.member` reachables
                       then PushStatusOk
                       else PushStatusGone

  pure . (uri,) . Right $ BulkPushResponse
    [ (nid, trgt, getstatus trgt) | (nid, trgt) <- notifs ]

flattenBulkPushRequest :: BulkPushRequest -> [(NotificationId, PushTarget)]
flattenBulkPushRequest (BulkPushRequest notifs) =
  mconcat $ (\(notif, trgts) -> (ntfId notif,) <$> trgts) <$> notifs
