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
    mkEnv = forAll (Pretty <$> resize len genMockEnv) mkPushes

    mkPushes :: Pretty MockEnv -> Property
    mkPushes (Pretty env) = forAllShrink
      (Pretty <$> resize len (genPushes (env ^. meRecipients)))
      shrinkPrettyPushes
      (prop env)

    prop :: MockEnv -> Pretty [Push] -> Property
    prop env (Pretty pushes) = foldl' (.&&.) (once True) props
      where
        ((), env') = runMockGundeck env (pushAll pushes)
        props = [ env' ^. meNativeQueue === expectNative
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
                go (Recipient uid _ cids{- may be [] -}, pay) = (\cid -> ((uid, cid), pay)) <$> cids

            removeSelf :: ((UserId, Maybe ClientId), (Recipient, Payload)) -> [(Recipient, Payload)]
            removeSelf ((_, Nothing), rcp@(Recipient _ _ (_:_), _)) =
              [rcp]
            removeSelf ((_, Just sender), (Recipient uid route cids@(_:_), pay)) =
              [(Recipient uid route $ filter (/= sender) cids, pay)]

            insertAllClients :: ((UserId, Maybe ClientId), (Recipient, Payload))
                             -> [((UserId, Maybe ClientId), (Recipient, Payload))]
            -- if sender and receiver are identical and own device in push is not set and the
            -- recipient client list is empty, send to no device.
            insertAllClients ((sender, Nothing), (Recipient receiver _ [], _)) | sender == receiver = []
            -- if the recipient client list is empty, but own device in push is set or sender and
            -- receiver differ, send to all devices except own device.
            insertAllClients (same@_, (rcp@(Recipient _ _ []), pay)) = [(same, (insertAllClients' rcp, pay))]
            -- otherwise, no special hidden meaning.
            insertAllClients same@(_, (Recipient _ _ (_:_), _)) = [same]

            insertAllClients' :: Recipient -> Recipient
            insertAllClients' same@(Recipient _ _ (_:_)) = same
            insertAllClients' (Recipient uid route []) = Recipient uid route cids
              where cids = maybe [] Map.keys . Map.lookup uid $ env ^. meNativeAddress

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
