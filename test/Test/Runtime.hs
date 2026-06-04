module Test.Runtime (tests) where

import Hwarden.Runtime (
  AgentPaths (..),
  deriveAgentPaths,
 )
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "runtime paths"
    [ testCase "deriveAgentPaths accepts the longest valid UNIX socket path" $
        deriveAgentPaths (baseRuntimeDirForSocketPathLength maxUnixSocketPathPathnameLength)
          @?= Right
            AgentPaths
              { runtimeDir = baseRuntimeDirForSocketPathLength maxUnixSocketPathPathnameLength
              , socketDir = baseRuntimeDirForSocketPathLength maxUnixSocketPathPathnameLength </> "hwarden"
              , socketPath =
                  baseRuntimeDirForSocketPathLength maxUnixSocketPathPathnameLength
                    </> "hwarden"
                    </> "agent.sock"
              , bitwardenCliAppDataDir =
                  baseRuntimeDirForSocketPathLength maxUnixSocketPathPathnameLength
                    </> "hwarden"
                    </> "bitwarden-cli"
              }
    , testCase "deriveAgentPaths rejects a UNIX socket path that is too long" $
        deriveAgentPaths (baseRuntimeDirForSocketPathLength (maxUnixSocketPathPathnameLength + 1))
          @?= Left "derived UNIX socket path is too long"
    ]

maxUnixSocketPathPathnameLength :: Int
maxUnixSocketPathPathnameLength = 107

baseRuntimeDirForSocketPathLength :: Int -> FilePath
baseRuntimeDirForSocketPathLength socketPathLength =
  replicate baseRuntimeDirLength 'x'
 where
  baseRuntimeDirLength =
    socketPathLength
      - length ("" </> "hwarden" </> "agent.sock")
      - 1
