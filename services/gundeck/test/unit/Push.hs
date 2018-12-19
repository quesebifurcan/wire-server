{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Push where

import Imports
import Control.Lens
import Data.Id
import Data.Range
import Data.String.Conversions (cs)
import Gundeck.Push (pushAll)
import Gundeck.Push.Websocket as Web (bulkPush)
import Gundeck.Types
import MockGundeck
import System.FilePath ((</>))
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "bulkpush" $
    ((\n -> testCase (show n) $ test n) <$> [1..4]) <>
    [ testProperty "web sockets" webBulkPushProps
    , testProperty "native pushes" pushAllProps
    ]


mkEnv :: (Pretty MockEnv -> Property) -> Positive Int -> Property
mkEnv prop (Positive len) = forAllShrink (Pretty <$> resize len genMockEnv) (shrinkPretty shrinkMockEnv) prop


testRootPath :: FilePath
testRootPath = "../mock-samples"

test :: Int -> Assertion
test i = do
  Just ((env, input) :: (MockEnv, [Push]))
    <- Aeson.decode <$> LBS.readFile (testRootPath </> show i <> ".json")
  runProp $ pushAllProp env (Pretty input)

runProp :: Property -> Assertion
runProp propty = quickCheckWithResult stdArgs { maxSuccess = 1, chatty = False } propty >>= \case
  Success {} -> pure ()
  bad@(Failure {}) -> assertBool (intercalate "\n" (failingTestCase bad) <> "\n" <> output bad) False
  bad -> assertBool (output bad) False


webBulkPushProps :: Positive Int -> Property
webBulkPushProps plen@(Positive len) = mkEnv mkNotifs plen
  where
    mkNotifs :: Pretty MockEnv -> Property
    mkNotifs (Pretty env) = forAllShrink
      (Pretty <$> resize len (genNotifs (env ^. meRecipients)))
      (shrinkPretty shrinkNotifs)
      (prop env)

    prop :: MockEnv -> Pretty [(Notification, [Presence])] -> Property
    prop env (Pretty notifs) = (sort . fst . runMockGundeck env $ Web.bulkPush notifs)
                           === (sort . fst . runMockGundeck env $ mockBulkPush notifs)


pushAllProps :: Positive Int -> Property
pushAllProps plen@(Positive len) = mkEnv mkPushes plen
  where
    mkPushes :: Pretty MockEnv -> Property
    mkPushes (Pretty env) = forAllShrink
      (Pretty <$> resize len (genPushes (env ^. meRecipients)))
      (shrinkPretty shrinkPushes)
      (pushAllProp env)

pushAllProp :: MockEnv -> Pretty [Push] -> Property
pushAllProp env (Pretty pushes) = counterexample (cs $ Aeson.encode (env, pushes))
                                $ foldl' (.&&.) (once True) props
  where
    ((), env') = runMockGundeck env (pushAll pushes)
    props = [ (Aeson.eitherDecode . Aeson.encode) pushes === Right pushes
            , (Aeson.eitherDecode . Aeson.encode) env === Right env
            , env' ^. meNativeQueue === expectNative
            ]

    expectNative :: Map (UserId, ClientId) Payload
    expectNative = Map.fromList
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

        removeSelf :: ((UserId, Maybe ClientId), (Recipient, Payload)) -> [(Recipient, Payload)]
        removeSelf ((sndr, Nothing), same@(Recipient rcp _ _, _)) =
          [same | sndr /= rcp]
        removeSelf ((_, Just sender), (Recipient uid route cids, pay)) =
          [(Recipient uid route $ filter (/= sender) cids, pay)]

        insertAllClients :: ((UserId, Maybe ClientId), (Recipient, Payload))
                         -> [((UserId, Maybe ClientId), (Recipient, Payload))]
        -- if the recipient client list is empty, fill in all devices of that user
        insertAllClients (same@_, (Recipient uid route [], pay)) = [(same, (rcp', pay))]
          where
            rcp' = Recipient uid route defaults
            defaults = maybe [] Map.keys . Map.lookup uid $ env ^. meNativeAddress

        -- otherwise, no special hidden meaning.
        insertAllClients same@(_, (Recipient _ _ (_:_), _)) = [same]

        rcps :: [((UserId, Maybe ClientId), (Recipient, Payload))]
        rcps = mconcat $ go <$> filter (not . (^. pushTransient)) pushes
          where
            go push = ((push ^. pushOrigin, clientIdFromConnId <$> push ^. pushOriginConnection),)
                    . (,push ^. pushPayload)
                  <$> (Set.toList . fromRange $ push ^. pushRecipients)


      -- TODO: meCassQueue (to be introduced) contains exactly those notifications that are
      -- non-transient.

      -- TODO: make Web.push mockable and actually receive messages sent over websocket via Chan,
      -- and store them in meWSQueue (to be introduced).  the test that messages expected to go over
      -- websocket actually will.

      -- TODO: test Presences with @ClientId = Nothing@

      -- TODO: test 'Route' more exhaustively.


      -- TODO: i think notifications go to sender via websocket right now.  should we turn that off, too?  (write a test)
