module Hwarden.Runtime (
  AgentPaths (..),
  deriveAgentPaths,
  resolveAgentPaths,
)
where

import System.Directory (XdgDirectory (XdgConfig), getXdgDirectory)
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute, (</>))

data AgentPaths = AgentPaths
  { runtimeDir :: FilePath
  , socketDir :: FilePath
  , socketPath :: FilePath
  , bitwardenCliAppDataDir :: FilePath
  }
  deriving (Eq, Show)

resolveAgentPaths :: FilePath -> IO (Either String AgentPaths)
resolveAgentPaths baseRuntimeDir = do
  appDataDir <- resolveBitwardenCliAppDataDir
  pure (appDataDir >>= deriveAgentPaths baseRuntimeDir)

resolveBitwardenCliAppDataDir :: IO (Either String FilePath)
resolveBitwardenCliAppDataDir = do
  override <- lookupEnv "BITWARDENCLI_APPDATA_DIR"
  case override of
    Just path
      | isAbsolute path -> pure (Right path)
      | otherwise -> pure (Left "BITWARDENCLI_APPDATA_DIR must be an absolute path")
    Nothing -> Right <$> getXdgDirectory XdgConfig ("hwarden" </> "bitwarden-cli")

deriveAgentPaths :: FilePath -> FilePath -> Either String AgentPaths
deriveAgentPaths baseRuntimeDir appDataDir =
  if length agentSocketPath > maxUnixSocketPathLength
    then Left "derived UNIX socket path is too long"
    else
      Right
        AgentPaths
          { runtimeDir = baseRuntimeDir
          , socketDir = hwardenRuntimeDir
          , socketPath = agentSocketPath
          , bitwardenCliAppDataDir = appDataDir
          }
 where
  hwardenRuntimeDir = baseRuntimeDir </> "hwarden"
  agentSocketPath = hwardenRuntimeDir </> "agent.sock"

maxUnixSocketPathLength :: Int
maxUnixSocketPathLength = 107
