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

import Imports
import Control.Lens
import Gundeck.Push (pushAll)
import Gundeck.Types.Push.V2
import MockGundeck
import Test.QuickCheck as QC
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck

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
    mkPushes env = forAll (resize len $ genPushes (env ^. mePresences)) (prop env)

    prop :: MockEnv -> [Push] -> Property
    prop env pushes = foldl' (.&&.) (once True) props
      where
        ((), env') = runMockGundeck env (pushAll pushes)
        props = [ env' ^. meWSQueue === Set.empty  -- TODO: this is silly
                , env' ^. meNativeQueue === Set.empty  -- TODO: this is silly, too
                ]

        -- TODO: less silly properties:
        -- - meWSQueue contains exactly those notifications that are non-transient.
        -- - meNativeQueue contains exactly those notifications that have not been sent to a WS.
        --   (currently, and i'm not sure that's correct: all notifications *not* sent to web
        --   sockets that return False in meWSReachable.)
        -- - ...
