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
import Data.Set as Set
import Gundeck.Push (pushAll)
import Gundeck.Types.Push.V2
import MockGundeck
import Test.QuickCheck as QC
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "PushAll"
    [ testProperty "works" pushAllProps
    ]

pushAllProps :: Positive Int -> Property
pushAllProps (Positive len) = mkEnv
  where
    mkEnv :: Property
    mkEnv = forAll (resize len genMockEnv) mkPushes

    mkPushes :: MockEnv -> Property
    mkPushes env = forAll (resize len $ genPushes (env ^. mePresences)) (prop env)

    prop :: MockEnv -> [Push] -> Property
    prop env pushes =
      let ((), env') = runMockGundeck env (pushAll pushes)
      in env' ^. meWSQueue === Set.empty
