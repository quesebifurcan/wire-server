{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Mock where

import Bilge
import Bilge.Assert
import Control.Arrow ((&&&))
import Control.Concurrent.Async       (Async, async, wait, forConcurrently_)
import Control.Lens                   ((.~), (^.), (^?), view, (<&>))
import Control.Retry                  (retrying, constantDelay, limitRetries)
import Data.Aeson              hiding (json)
import Data.Aeson.Lens
import Data.ByteString.Conversion
import Data.ByteString.Lazy           (fromStrict)
import Data.Id
import Data.List1                     (List1)
import Data.Range
import Data.UUID.V4
import Gundeck.Monad
import Gundeck.Push
import Gundeck.Push.Native              as Native
import Gundeck.Push.Native.Types
import Gundeck.Types
import Imports
import Network.URI                    (parseURI)
import Safe
import System.Logger                    as Log
import System.Random                  (randomIO)
import System.Timeout                 (timeout)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Cassandra              as Cql
import qualified Data.Aeson.Types       as Aeson
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as C
import qualified Data.ByteString.Lazy   as BL
import qualified Data.HashMap.Strict    as HashMap
import qualified Data.List1             as List1
import qualified Data.Set               as Set
import qualified Data.Text.Encoding     as T
import qualified Data.UUID              as UUID
import qualified Gundeck.Push.Data      as Push
import qualified Network.HTTP.Client    as Http
import qualified Network.WebSockets     as WS
import qualified Prelude


newtype MockGundeck a = MockGundeck { fromMockGundeck :: StateT MockEnv (ExceptT String Identity) a }
  deriving (Functor, Applicative, Monad, MonadState MockEnv, MonadError String)

data MockEnv = MockEnv
  { _meStdGen    :: StdGen
  , _mePresences :: [(UserId, [MockPresence])]
  , _meQueue     :: Set NotificationId
  }

data MockPresence = MockPresence
  { _mprPresence  :: Presence
  , _mprReachable :: Bool
  , _mprAddress   :: Maybe (Address "no-keys")
  }

makeLenses 'MockEnv
makeLenses 'MockPresence

instance MonadThrow MockGundeck where
  throwM = error . show  -- (this is a bit lazy, yes.)

instance MonadPushAll MockGundeck where
  mpaNotificationTTL = pure $ NotificationTTL 300  -- (doesn't matter.)
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

-- | (There is certainly a fancier implementation using '<%=' or similar, but this one is easier to
-- reverse engineer later.)
mockMkNotificationId :: m ~ MockGundeck => m NotificationId
mockMkNotificationId = Id <$> state go
  where
    go env = case random (env ^. meStdGen) of
      (uuid, g') -> (uuid, env & meStdGen .~ g')

mockListAllPresences :: m ~ MockGundeck => [UserId] -> m [[Presence]]
mockListAllPresences uids = do
  hits :: [(UserId, [MockPresence])] <- filter ((`elem` uids) . fst) <$> gets (^. mePresences)
  pure $ (^. mprPresence) <$$> (snd <$> hits)

mockBulkPush :: m ~ MockGundeck => [(Notification, [Presence])] -> m [(NotificationId, [Presence])]
mockBulkPush notifs = do

  -- TODO: do we need to remove successful deliveries from the stream in the mock state?

  good :: [Presence] <- (^. mprPresence) <$$> (filter (^. mprReachable) . mconcat <$> (snd <$$> gets (^. mePresences)))
  pure [ (nid, prcs)
       | (ntfId -> nid, filter (`elem` good) -> prcs) <- notifs
       , not $ null prcs
       ]

mockStreamAdd :: m ~ MockGundeck => NotificationId -> List1 NotificationTarget -> List1 Aeson.Object -> NotificationTTL -> m ()
mockStreamAdd nid _ _ _ = modify $ meQueue %~ Set.insert nid

mockPushNative :: m ~ MockGundeck => Notification -> Push -> [Address "no-keys"] -> m ()
mockPushNative = _  -- what is this supposed to do?

mockLookupAddress :: m ~ MockGundeck => UserId -> m [Address "no-keys"]
mockLookupAddress uid = do
  users :: [(UserId, [MockPresence])] <- gets (^. mePresences)
  mockprcs :: [MockPresence] <- maybe (error "user not found!") pure $ lookup uid users
  pure . catMaybes $ (^. mprAddress) <$> mockprcs


