module Test.Runtime (tests) where

import Control.Exception (bracket)
import Hwarden.Runtime (
  AgentPaths (..),
  deriveAgentPaths,
  resolveAgentPaths,
 )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "runtime paths"
    [ testCase "deriveAgentPaths accepts the longest valid UNIX socket path" $
        deriveAgentPaths
          (baseRuntimeDirForSocketPathLength maxUnixSocketPathPathnameLength)
          persistentBitwardenCliAppDataDir
          @?= Right
            AgentPaths
              { runtimeDir = baseRuntimeDirForSocketPathLength maxUnixSocketPathPathnameLength
              , socketDir = baseRuntimeDirForSocketPathLength maxUnixSocketPathPathnameLength </> "hwarden"
              , socketPath =
                  baseRuntimeDirForSocketPathLength maxUnixSocketPathPathnameLength
                    </> "hwarden"
                    </> "agent.sock"
              , bitwardenCliAppDataDir = persistentBitwardenCliAppDataDir
              }
    , testCase "deriveAgentPaths keeps Bitwarden CLI appdata outside the runtime directory" $
        deriveAgentPaths "/run/user/1000" persistentBitwardenCliAppDataDir
          @?= Right
            AgentPaths
              { runtimeDir = "/run/user/1000"
              , socketDir = "/run/user/1000" </> "hwarden"
              , socketPath = "/run/user/1000" </> "hwarden" </> "agent.sock"
              , bitwardenCliAppDataDir = persistentBitwardenCliAppDataDir
              }
    , testCase "deriveAgentPaths rejects a UNIX socket path that is too long" $
        deriveAgentPaths
          (baseRuntimeDirForSocketPathLength (maxUnixSocketPathPathnameLength + 1))
          persistentBitwardenCliAppDataDir
          @?= Left "derived UNIX socket path is too long"
    , testCase "resolveAgentPaths respects an absolute Bitwarden CLI appdata override" $
        withEnvVarOverride "BITWARDENCLI_APPDATA_DIR" (Just "/var/lib/hwarden/bitwarden-cli") $
          withEnvVarOverride "XDG_CONFIG_HOME" Nothing $
            withEnvVarOverride "HOME" (Just "/home/alice") $
              do
                paths <- resolveAgentPaths "/run/user/1000"
                paths
                  @?= Right
                    AgentPaths
                      { runtimeDir = "/run/user/1000"
                      , socketDir = "/run/user/1000" </> "hwarden"
                      , socketPath = "/run/user/1000" </> "hwarden" </> "agent.sock"
                      , bitwardenCliAppDataDir = "/var/lib/hwarden/bitwarden-cli"
                      }
    , testCase "resolveAgentPaths rejects a relative Bitwarden CLI appdata override" $
        withEnvVarOverride "BITWARDENCLI_APPDATA_DIR" (Just "relative/bitwarden-cli") $
          do
            paths <- resolveAgentPaths "/run/user/1000"
            paths @?= Left "BITWARDENCLI_APPDATA_DIR must be an absolute path"
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

persistentBitwardenCliAppDataDir :: FilePath
persistentBitwardenCliAppDataDir =
  "/home/alice/.config" </> "hwarden" </> "bitwarden-cli"

withEnvVarOverride :: String -> Maybe String -> IO a -> IO a
withEnvVarOverride key newValue action = do
  oldValue <- lookupEnv key
  bracket
    (setOverride key newValue)
    (\() -> restoreOverride key oldValue)
    (\() -> action)
 where
  setOverride envKey value =
    case value of
      Just envValue -> setEnv envKey envValue
      Nothing -> unsetEnv envKey
  restoreOverride envKey value =
    case value of
      Just envValue -> setEnv envKey envValue
      Nothing -> unsetEnv envKey
