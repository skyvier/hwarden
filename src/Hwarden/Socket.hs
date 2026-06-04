module Hwarden.Socket (recvAll) where

import qualified Data.ByteString as BS
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as NBS

recvAll :: Socket -> IO BS.ByteString
recvAll conn = go []
 where
  go acc = do
    chunk <- NBS.recv conn 4096
    if BS.null chunk
      then pure (BS.concat (reverse acc))
      else go (chunk : acc)
