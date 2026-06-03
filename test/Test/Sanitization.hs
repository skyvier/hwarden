module Test.Sanitization (tests) where

import Test.Tasty (TestTree, testGroup)

import qualified Test.Sanitization.Json as Json
import qualified Test.Sanitization.Log as Log
import qualified Test.Sanitization.Show as Show

tests :: TestTree
tests = testGroup "sanitization"
  [ Json.tests
  , Show.tests
  , Log.tests
  ]
