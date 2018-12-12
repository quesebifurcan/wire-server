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
-- import Control.Lens
-- import Control.Monad.Catch
-- import Control.Monad.Except
-- import Control.Monad.State
-- import Data.Id
-- import Data.List1                     (List1)
-- import Data.Misc ((<$$>))
-- import Gundeck.Aws.Arn as Aws
-- import Gundeck.Options
-- import Gundeck.Push
-- import Gundeck.Push.Native              as Native
-- import Gundeck.Types
-- import MockGundeck
-- import qualified Data.Aeson.Types       as Aeson
-- import qualified Data.Set as Set
-- import qualified Network.URI as URI
-- import System.Random
import Test.QuickCheck as QC
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "PushAll"
    [ testProperty "works" pushAllProps
    ]

pushAllProps :: Positive Int -> Property
pushAllProps = undefined

{-
(Positive l) = ioProperty $ do
    q  <- DelayQueue.new (Clock (return 1)) (Delay 1) (Limit l)
    r  <- forM [1..l+1] $ \(i :: Int) -> DelayQueue.enqueue q i i
    l' <- DelayQueue.length q
    return $ r  == replicate l True ++ [False]
          && l' == l
-}

-- pushAll :: MonadPushAll m => [Push] -> m ()

-- generate a bunch of pushes, together with info on which web socket transmissions should fail.
-- then run pushall, and retrieve the information what was sent over web socket and what was sent
-- via push.
