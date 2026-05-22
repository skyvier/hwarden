module Main where

import Hwarden.Agent (runAgent)
import System.Environment (getArgs, lookupEnv)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["version"] -> do
      maybeVersion <- lookupEnv "HWARDEN_VERSION"
      putStrLn (maybe "unknown" id maybeVersion)
    _ -> runAgent
