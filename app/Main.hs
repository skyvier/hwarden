{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.MVar (MVar, newMVar, swapMVar)
import Control.Exception (SomeException, catch, finally)
import Control.Monad (forever, void, when)
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    encode,
    eitherDecodeStrict',
    object,
    withObject,
    (.:),
    (.=)
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket
  ( Family (AF_UNIX),
    Socket,
    SockAddr (SockAddrUnix),
    SocketType (Stream),
    accept,
    bind,
    close,
    defaultProtocol,
    listen,
    socket
  )
import qualified Network.Socket.ByteString as NBS
import System.Directory
  ( createDirectoryIfMissing,
    doesPathExist,
    removePathForcibly
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), die)
import System.FilePath ((</>))
import System.Posix.Files (ownerModes, setFileMode)
import System.Process (readProcessWithExitCode)

newtype SessionKey = SessionKey Text

data Request
  = UnlockRequest Text Text
  | UnknownRequest

data Response
  = Success Text
  | Failure Text

instance FromJSON Request where
  parseJSON = withObject "Request" $ \obj -> do
    cmd <- obj .: "cmd"
    case (cmd :: Text) of
      "unlock" -> UnlockRequest <$> obj .: "email" <*> obj .: "password"
      _ -> pure UnknownRequest

instance ToJSON Response where
  toJSON (Success message) =
    object
      [ "ok" .= True,
        "message" .= message
      ]
  toJSON (Failure err) =
    object
      [ "ok" .= False,
        "error" .= err
      ]

main :: IO ()
main = do
  runtimeDir <- requireRuntimeDir
  let socketDir = runtimeDir </> "hwarden"
      socketPath = socketDir </> "agent.sock"

  prepareSocketDir socketDir
  removeExistingSocket socketPath

  sessionVar <- newMVar Nothing
  sock <- socket AF_UNIX Stream defaultProtocol
  finally
    (do
        bind sock (SockAddrUnix socketPath)
        listen sock 5
        forever $ do
          (conn, _) <- accept sock
          handleConnection sessionVar conn)
    (close sock `finally` cleanupSocket socketPath)

requireRuntimeDir :: IO FilePath
requireRuntimeDir = do
  runtimeDir <- lookupEnv "XDG_RUNTIME_DIR"
  case runtimeDir of
    Just path -> pure path
    Nothing -> die "XDG_RUNTIME_DIR is not set"

prepareSocketDir :: FilePath -> IO ()
prepareSocketDir socketDir = do
  createDirectoryIfMissing True socketDir
  setFileMode socketDir ownerModes

removeExistingSocket :: FilePath -> IO ()
removeExistingSocket socketPath = do
  exists <- doesPathExist socketPath
  whenExists exists $ removePathForcibly socketPath

cleanupSocket :: FilePath -> IO ()
cleanupSocket socketPath =
  catch (removeExistingSocket socketPath) ignoreException

handleConnection :: MVar (Maybe SessionKey) -> Socket -> IO ()
handleConnection sessionVar conn =
  finally
    (do
        raw <- recvAll conn
        response <-
          case eitherDecodeStrict' raw of
            Left err -> pure (Failure (T.pack err))
            Right request -> handleRequest sessionVar request
        NBS.sendAll conn (LBS.toStrict (encodeResponse response)))
    (close conn)

handleRequest :: MVar (Maybe SessionKey) -> Request -> IO Response
handleRequest sessionVar request =
  case request of
    UnlockRequest email password -> unlock sessionVar email password
    UnknownRequest -> pure (Failure "unknown command")

unlock :: MVar (Maybe SessionKey) -> Text -> Text -> IO Response
unlock sessionVar email password = do
  let args = [T.unpack email, T.unpack password, "--raw"]
  result <-
    catch
      (Just <$> readProcessWithExitCode "bw" ("login" : args) "")
      ignoreProcessException
  case result of
    Nothing ->
      pure (Failure "bw login failed")
    Just (exitCode, stdoutText, stderrText) ->
      case exitCode of
        ExitSuccess -> do
          let sessionKey = SessionKey (T.strip (T.pack stdoutText))
          void (swapMVar sessionVar (Just sessionKey))
          pure (Success "unlocked")
        ExitFailure _ ->
          pure (Failure (sanitizeError password (T.pack stderrText)))

recvAll :: Socket -> IO BS.ByteString
recvAll conn = go []
  where
    go acc = do
      chunk <- NBS.recv conn 4096
      if BS.null chunk
        then pure (BS.concat (reverse acc))
        else go (chunk : acc)

encodeResponse :: Response -> LBS.ByteString
encodeResponse = encode

sanitizeError :: Text -> Text -> Text
sanitizeError password err =
  let trimmed = T.strip (T.replace password "<redacted>" err)
   in if T.null trimmed then "bw login failed" else trimmed

ignoreException :: SomeException -> IO ()
ignoreException _ = pure ()

ignoreProcessException :: SomeException -> IO (Maybe (ExitCode, String, String))
ignoreProcessException _ = pure Nothing

whenExists :: Bool -> IO () -> IO ()
whenExists exists action = when exists action
