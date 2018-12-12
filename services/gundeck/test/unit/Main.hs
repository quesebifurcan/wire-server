module Main (main) where

import Imports
import OpenSSL (withOpenSSL)
import Test.Tasty

import qualified DelayQueue
import qualified Json
import qualified Mock
import qualified Native

main :: IO ()
main = withOpenSSL . defaultMain $
    testGroup "Main"
        [ DelayQueue.tests
        , Json.tests
        , Mock.tests
        , Native.tests
        ]
