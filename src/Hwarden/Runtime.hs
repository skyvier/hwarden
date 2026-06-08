module Hwarden.Runtime (
  AgentPaths (..),
  deriveAgentPaths,
  resolveAgentPaths,
)
where

import System.Directory (XdgDirectory (XdgConfig), getXdgDirectory)
import System.FilePath ((</>))

data AgentPaths = AgentPaths
  { runtimeDir :: FilePath
  , socketDir :: FilePath
  , socketPath :: FilePath
  , bitwardenCliAppDataDir :: FilePath
  }
  deriving (Eq, Show)

resolveAgentPaths :: FilePath -> IO (Either String AgentPaths)
resolveAgentPaths baseRuntimeDir = do
  appDataDir <- getXdgDirectory XdgConfig ("hwarden" </> "bitwarden-cli")
  pure (deriveAgentPaths baseRuntimeDir appDataDir)

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
