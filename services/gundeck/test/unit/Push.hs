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

module Push where

import Control.Lens
import Data.Id
import Data.Range
import Gundeck.Push (pushAll)
import Gundeck.Types
import Imports
import MockGundeck
import Test.QuickCheck as QC
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.Map as Map
import qualified Data.Set as Set


tests :: TestTree
tests = testGroup "PushAll"
    [ testProperty "works" pushAllProps
    ]

-- | NB: shrinking is not implemented yet.
pushAllProps :: Positive Int -> Property
pushAllProps (Positive len) = mkEnv
  where
    mkEnv :: Property
    mkEnv = forAll (resize len genMockEnv) mkPushes

    mkPushes :: MockEnv -> Property
    mkPushes env = forAllShrink (resize len $ genPushes (env ^. meRecipients)) shrinkPushes (prop env)

    prop :: MockEnv -> [Push] -> Property
    prop env pushes = foldl' (.&&.) (once True) props
      where
        ((), env') = runMockGundeck env (pushAll pushes)
        props = [ env' ^. meNativeQueue === expectNative
                ]

        expectNative :: Map (UserId, ClientId) Payload
        expectNative = Map.fromList reachables
          where
            reachables :: [((UserId, ClientId), Payload)]
            reachables = filter (reachableNative . fst)
                       . filter (not . reachableWS . fst)
                       . mconcat
                       $ go <$> rcps
              where
                go :: (Recipient, Payload) -> [((UserId, ClientId), Payload)]
                go (Recipient uid _ cids, pay) = (\cid -> ((uid, cid), pay)) <$> cids

                reachableWS :: (UserId, ClientId) -> Bool
                reachableWS = (`elem` (env ^. meWSReachable))

                reachableNative :: (UserId, ClientId) -> Bool
                reachableNative (uid, cid) = maybe False (`elem` (env ^. meNativeReachable)) adr
                  where adr = (Map.lookup uid >=> Map.lookup cid) (env ^. meNativeAddress)

            rcps :: [(Recipient, Payload)]
            rcps = mconcat $ go <$> filter (not . (^. pushTransient)) pushes
              where
                go :: Push -> [(Recipient, Payload)]
                go push = (,push ^. pushPayload) <$> (Set.toList . fromRange $ push ^. pushRecipients)



      -- TODO: meCassQueue (to be introduced) contains exactly those notifications that are
      -- non-transient.

      -- TODO: make Web.push mockable and actually receive messages sent over websocket via Chan,
      -- and store them in meWSQueue (to be introduced).  the test that messages expected to go over
      -- websocket actually will.
