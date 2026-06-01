module Hwarden.Runtime
  ( AgentPaths (..),
    deriveAgentPaths,
  )
where

import System.FilePath ((</>))

data AgentPaths = AgentPaths
  { runtimeDir :: FilePath,
    socketDir :: FilePath,
    socketPath :: FilePath,
    bitwardenCliAppDataDir :: FilePath
  }
  deriving (Eq, Show)

deriveAgentPaths :: FilePath -> Either String AgentPaths
deriveAgentPaths baseRuntimeDir =
  if length agentSocketPath > maxUnixSocketPathLength
    then Left "derived UNIX socket path is too long"
    else
      Right
        AgentPaths
          { runtimeDir = baseRuntimeDir,
            socketDir = hwardenRuntimeDir,
            socketPath = agentSocketPath,
            bitwardenCliAppDataDir = hwardenRuntimeDir </> "bitwarden-cli"
          }
  where
    hwardenRuntimeDir = baseRuntimeDir </> "hwarden"
    agentSocketPath = hwardenRuntimeDir </> "agent.sock"

maxUnixSocketPathLength :: Int
maxUnixSocketPathLength = 107
