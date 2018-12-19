{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-orphans #-}

module MockGundeck where

import Imports
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.State
import Data.Aeson
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
import System.Logger.Class as Log
import System.Random
import Test.QuickCheck as QC
import Test.QuickCheck.Instances ()

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Network.URI as URI


----------------------------------------------------------------------
-- helpers (move to some more general-purpose test library?)

-- | (it may be possible to drop this type in favor of more sophisticated use of quickcheck's
-- counterexample.)
newtype Pretty a = Pretty a
  deriving (Eq, Ord)

instance Aeson.ToJSON a => Show (Pretty a) where
  show (Pretty a) = cs $ Aeson.encodePretty a


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
  deriving (Eq, Show)

instance Eq StdGen where
  (==) = (==) `on` show

makeLenses ''MockEnv

-- (serializing test cases makes replay easier.)
instance ToJSON MockEnv where
  toJSON env = object
    [ "meStdGen"          Aeson..= show (env ^. meStdGen)
    , "meRecipients"      Aeson..= (env ^. meRecipients)
    , "meNativeAddress"   Aeson..= Map.toList (Map.toList <$> (env ^. meNativeAddress))
    , "meWSReachable"     Aeson..= Set.toList (env ^. meWSReachable)
    , "meNativeReachable" Aeson..= Set.toList (env ^. meNativeReachable)
    , "meNativeQueue"     Aeson..= Map.toList (env ^. meNativeQueue)
    ]

instance ToJSON (Address s) where
  toJSON adr = Aeson.object
    [ "addrUser"      Aeson..= (adr ^. addrUser)
    , "addrTransport" Aeson..= (adr ^. addrTransport)
    , "addrApp"       Aeson..= (adr ^. addrApp)
    , "addrToken"     Aeson..= (adr ^. addrToken)
    , "addrEndpoint"  Aeson..= (serializeFakeAddrEndpoint $ adr ^. addrEndpoint)
    , "addrConn"      Aeson..= (adr ^. addrConn)
    , "addrClient"    Aeson..= (adr ^. addrClient)
    ]

serializeFakeAddrEndpoint :: EndpointArn -> (Text, Transport, AppName)
serializeFakeAddrEndpoint ((^. snsTopic) -> eptopic) =
  ( case eptopic ^. endpointId of EndpointId txt -> txt
  , eptopic ^. endpointTransport
  , eptopic ^. endpointAppName
  )

instance FromJSON MockEnv where
  parseJSON = withObject "MockEnv" $ \env -> MockEnv
    <$> (read <$> (env Aeson..: "meStdGen"))
    <*> (env Aeson..: "meRecipients")
    <*> (Map.fromList <$$> (Map.fromList <$> (env Aeson..: "meNativeAddress")))
    <*> (Set.fromList <$> (env Aeson..: "meWSReachable"))
    <*> (Set.fromList <$> (env Aeson..: "meNativeReachable"))
    <*> (Map.fromList <$> (env Aeson..: "meNativeQueue"))

instance FromJSON (Address s) where
  parseJSON = withObject "Address" $ \adr -> Address
    <$> (adr Aeson..: "addrUser")
    <*> (adr Aeson..: "addrTransport")
    <*> (adr Aeson..: "addrApp")
    <*> (adr Aeson..: "addrToken")
    <*> (mkFakeAddrEndpoint <$> adr Aeson..: "addrEndpoint")
    <*> (adr Aeson..: "addrConn")
    <*> (adr Aeson..: "addrClient")

mkFakeAddrEndpoint :: (Text, Transport, AppName) -> EndpointArn
mkFakeAddrEndpoint (epid, transport, app) = Aws.mkSnsArn Tokyo (Account "acc") eptopic
  where eptopic = mkEndpointTopic (ArnEnv "") transport app (EndpointId epid)


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
          go' addr = Just . maybe newEntries (newEntries <>)
            where newEntries = Map.fromList [(addr ^. addrClient, addr)]

  _meWSReachable <- genPredicate . mconcat $ recipientToIds <$> _meRecipients
  _meNativeReachable <- genPredicate (snd <$> addrs)

  let _meNativeQueue = mempty
      env = MockEnv {..}

  validateMockEnv env & either error (const $ pure env)

shrinkMockEnv :: MockEnv -> [MockEnv]
shrinkMockEnv env = do
  rcps :: [Recipient] <- shrinkList (const []) (env ^. meRecipients)

  let env' :: MockEnv
      env' = env
        & meRecipients .~ rcps
        & meNativeAddress %~ naddrs
        & meWSReachable %~ wsrchbl
        & meNativeReachable %~ ntrchbl

      clients :: Set (UserId, ClientId)
      clients = Set.fromList . mconcat $ (\(Recipient uid _ cids) -> (uid,) <$> cids) <$> rcps

      naddrs  = Map.filterWithKey (\uid _ -> uid `Set.member` (fst `Set.map` clients))
              . fmap (Map.filterWithKey (\cid _ -> cid `Set.member` (snd `Set.map` clients)))
      wsrchbl = Set.filter (`Set.member` clients)
      ntrchbl = Set.filter (`Set.member` flatten (env' ^. meNativeAddress))
        where flatten = Set.fromList . mconcat . Map.elems . fmap Map.elems

  [env' | not $ null rcps]

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
  uid  <- genId
  cids <- listOf1 genClientId
  pure $ Recipient uid RouteAny cids

genId :: Gen (Id a)
genId = do
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
  arnEpId <- arbitrary
  let _addrApp = "AppName"
      _addrToken = Token "tok"
      _addrEndpoint = mkFakeAddrEndpoint (arnEpId, _addrTransport, _addrApp)
  pure $ \_addrUser _addrClient -> let _addrConn = fakeConnId _addrClient in Address {..}

genPushes :: [Recipient] -> Gen [Push]
genPushes = listOf . genPush

shrinkPushes :: [Push] -> [[Push]]
shrinkPushes = shrinkList shrinkPush
  where
    shrinkPush :: Push -> [Push]
    shrinkPush psh = (\rcps -> psh & pushRecipients .~ rcps) <$> shrinkRecipients (psh ^. pushRecipients)

    shrinkRecipients :: Range 1 1024 (Set Recipient) -> [Range 1 1024 (Set Recipient)]
    shrinkRecipients = fmap unsafeRange . map Set.fromList . filter (not . null) . shrinkList shrinkRecipient . Set.toList . fromRange

    shrinkRecipient :: Recipient -> [Recipient]
    shrinkRecipient _ = []

shrinkPretty :: (a -> [a]) -> Pretty a -> [Pretty a]
shrinkPretty shrnk (Pretty xs) = Pretty <$> shrnk xs

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
genPayload = do
  num :: Int <- arbitrary
  pure $ List1 (HashMap.singleton "val" (Aeson.toJSON num) NE.:| [])

instance Arbitrary Aeson.Value where
  -- (not currently in use; 'genPayload' is built to be more compact.)
  arbitrary = oneof
    [ Aeson.object <$> listOf ((Aeson..=) <$> arbitrary <*> (scale (`div` 3) (arbitrary @Aeson.Value)))
    , Aeson.Array . Vector.fromList <$> listOf (scale (`div` 3) (arbitrary @Aeson.Value))
    , Aeson.String <$> arbitrary
    , Aeson.Number <$> arbitrary
    , Aeson.Bool <$> QC.elements [minBound..]
    , pure Aeson.Null
    ]

genNotif :: Gen Notification
genNotif = Notification <$> genId <*> arbitrary <*> genPayload

genNotifs :: [Recipient] -> Gen [(Notification, [Presence])]
genNotifs (mconcat . fmap fakePresences -> allprcs) = fmap uniqNotifs . listOf $ do
  notif <- genNotif
  prcs <- listOf $ QC.elements allprcs
  pure (notif, prcs)
  where
    uniqNotifs = nubBy ((==) `on` (ntfId . fst)) . fmap (_2 %~ nub)

shrinkNotifs :: [(Notification, [Presence])] -> [[(Notification, [Presence])]]
shrinkNotifs = shrinkList (\(notif, prcs) -> (notif,) <$> shrinkList (const []) prcs)


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

-- | Expected behavior of 'Gundeck.Push.pushAll' (used in the property test).
mockPushAll
  :: (HasCallStack, m ~ MockGundeck)
  => [Push] -> m ()
mockPushAll pushes = modify $ \env -> env & meNativeQueue .~ expectNative env
  where
    expectNative :: MockEnv -> Map (UserId, ClientId) Payload
    expectNative env = Map.fromList
                 . filter reachable
                 . reformat
                 . mconcat . fmap removeSelf
                 . mconcat . fmap insertAllClients
                 $ rcps
      where
        reachable :: ((UserId, ClientId), payload) -> Bool
        reachable (ids, _) = reachableNative ids && not (reachableWS ids)
          where
            reachableWS :: (UserId, ClientId) -> Bool
            reachableWS = (`elem` (env ^. meWSReachable))

            reachableNative :: (UserId, ClientId) -> Bool
            reachableNative (uid, cid) = maybe False (`elem` (env ^. meNativeReachable)) adr
              where adr = (Map.lookup uid >=> Map.lookup cid) (env ^. meNativeAddress)

        reformat :: [(Recipient, Payload)] -> [((UserId, ClientId), Payload)]
        reformat = mconcat . fmap go
          where
            go (Recipient uid _ cids, pay) = (\cid -> ((uid, cid), pay)) <$> cids

        removeSelf :: ((UserId, Maybe ClientId, Bool), (Recipient, Payload)) -> [(Recipient, Payload)]
        removeSelf ((snduid, _, False), same@(Recipient rcpuid _ _, _)) =
          -- if pushNativeIncludeOrigin is False, none of the originator's devices will receive
          -- native pushes.
          [same | snduid /= rcpuid]
        removeSelf ((_, Just sndcid, True), (Recipient rcpuid route cids, pay)) =
          -- if originating client is known and pushNativeIncludeOrigin is True, filter out just
          -- that client, and native-push to all other devices of the originator.
          [(Recipient rcpuid route $ filter (/= sndcid) cids, pay)]
        removeSelf ((_, Nothing, True), same) =
          -- if no originating client is given and pushNativeIncludeOrigin is True, push to all
          -- originating devices.
          [same]

        insertAllClients :: (any, (Recipient, Payload))
                         -> [(any, (Recipient, Payload))]
        -- if the recipient client list is empty, fill in all devices of that user
        insertAllClients (same, (Recipient uid route [], pay)) = [(same, (rcp', pay))]
          where
            rcp' = Recipient uid route defaults
            defaults = maybe [] Map.keys . Map.lookup uid $ env ^. meNativeAddress

        -- otherwise, no special hidden meaning.
        insertAllClients same@(_, (Recipient _ _ (_:_), _)) = [same]

        rcps :: [((UserId, Maybe ClientId, Bool), (Recipient, Payload))]
        rcps = mconcat $ go <$> filter (not . (^. pushTransient)) pushes
          where
            go psh = (( psh ^. pushOrigin
                      , clientIdFromConnId <$> psh ^. pushOriginConnection
                      , psh ^. pushNativeIncludeOrigin
                      ),)
                   . (,psh ^. pushPayload)
                  <$> (Set.toList . fromRange $ psh ^. pushRecipients)


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
mockBulkSend uri notifs = do
  reachables :: Set PushTarget
    <- Set.map (uncurry PushTarget . (_2 %~ fakeConnId)) <$> gets (^. meWSReachable)
  let getstatus trgt = if trgt `Set.member` reachables
                       then PushStatusOk
                       else PushStatusGone

      flattenBulkPushRequest :: BulkPushRequest -> [(NotificationId, PushTarget)]
      flattenBulkPushRequest (BulkPushRequest ntifs) =
        mconcat $ (\(ntif, trgts) -> (ntfId ntif,) <$> trgts) <$> ntifs

  pure . (uri,) . Right $ BulkPushResponse
    [ (nid, trgt, getstatus trgt) | (nid, trgt) <- flattenBulkPushRequest notifs ]
