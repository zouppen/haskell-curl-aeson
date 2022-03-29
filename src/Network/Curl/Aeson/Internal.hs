{-# LANGUAGE OverloadedStrings #-}
module Network.Curl.Aeson.Internal where

import Data.IORef
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as BL
import Foreign.Ptr
import Network.Curl.Opts (ReadFunction)

-- |Creates ReadFunction which feeds strict ByteString.
mkReadFunction :: B.ByteString -> IO ReadFunction
mkReadFunction bs = feeder <$> newIORef [bs]

-- |Creates ReadFunction which feeds lazy ByteString.
mkReadFunctionLazy :: BL.ByteString -> IO ReadFunction
mkReadFunctionLazy bs = feeder <$> newIORef (BL.toChunks bs)

-- |ReadFunction for curl to feed in the payload
feeder :: IORef [B.ByteString] -> ReadFunction --Ptr CChar -> CInt -> CInt -> Ptr () -> IO (Maybe CInt)
feeder input destPtr size nitems _ = do
  -- Take next chunk
  chunk <- atomicModifyIORef input (takeChunk $ fromIntegral destLen)
  -- Now doing the hard copying
  if B.null chunk
    then pure Nothing
    else do B.unsafeUseAsCStringLen chunk $ \(srcPtr, srcLen) ->
              B.memcpy (castPtr destPtr) (castPtr srcPtr) (fromIntegral destLen)
            pure $ Just destLen
  where destLen = size*nitems

takeChunk :: Int -> [B.ByteString] -> ([B.ByteString], B.ByteString)
takeChunk len [] = ([], "")
takeChunk len ("":xs) = takeChunk len xs
takeChunk len (chunk:xs) =
  let (now, later) = B.splitAt len chunk
  in (later:xs, now)
