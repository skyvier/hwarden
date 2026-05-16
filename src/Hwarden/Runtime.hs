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

deriveAgentPaths :: FilePath -> AgentPaths
deriveAgentPaths baseRuntimeDir =
  AgentPaths
    { runtimeDir = baseRuntimeDir,
      socketDir = hwardenRuntimeDir,
      socketPath = hwardenRuntimeDir </> "agent.sock",
      bitwardenCliAppDataDir = hwardenRuntimeDir </> "bitwarden-cli"
    }
  where
    hwardenRuntimeDir = baseRuntimeDir </> "hwarden"
