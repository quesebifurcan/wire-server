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

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS


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
  bad@(Failure {}) -> assertBool (intercalate "\n" (failingTestCase bad)) False
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
    ((), env')  = runMockGundeck env (pushAll pushes)
    ((), env'') = runMockGundeck env (mockPushAll pushes)
    props = [ (Aeson.eitherDecode . Aeson.encode) pushes === Right pushes
            , (Aeson.eitherDecode . Aeson.encode) env === Right env
            , env' ^. meNativeQueue === env'' ^. meNativeQueue
            ]


      -- TODO: test: meCassQueue (to be introduced) contains exactly those notifications that are
      --       non-transient.

      -- TODO: test: meWSQueue (to be introduced contains websocket deliveries.

      -- TODO: test Presences with @ClientId = Nothing@

      -- TODO: test 'Route' more exhaustively.

      -- TODO: newPush doesn't cover a lot of the domain of the 'Push' type.  figure out what the
      --       actually expected values are, and constrain the type accordingly.  if we need to be
      --       downwards compatible perhaps we can do that in the json parsers.
