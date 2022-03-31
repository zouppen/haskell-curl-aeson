{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module    : Network.Curl.Aeson.Internal
-- Copyright : (c) 2022, Joel Lehtonen
-- License   : BSD3
--
-- Maintainer: Joel Lehtonen <joel.lehtonen+curlaeson@iki.fi>
-- Stability : experimental
-- Portability: portable
--
-- Internal support functions to for uploading ByteString payload with
-- libcurl.
module Network.Curl.Aeson.Internal ( mkReadFunction
                                   , mkReadFunctionLazy
                                   ) where

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
feeder :: IORef [B.ByteString] -> ReadFunction
feeder input dst size nitems _ = do
  -- Take next chunk
  chunk <- atomicModifyIORef input $ takeChunk $ fromIntegral $ size*nitems
  -- Now doing the hard copying
  B.unsafeUseAsCStringLen chunk rawCopy
  pure $ Just $ fromIntegral $ B.length chunk
  where rawCopy (src, srcLen) = B.memcpy (castPtr dst) (castPtr src) srcLen

-- |Takes chunk of size 1 to /len/ bytes. In case the chunk list is
-- empty, it returns zero-length string. May give unnecessarily short
-- chunks in case of small chunks way smaller than buffer.
takeChunk :: Int -> [B.ByteString] -> ([B.ByteString], B.ByteString)
takeChunk len [] = ([], "")
takeChunk len ("":xs) = takeChunk len xs
takeChunk len (chunk:xs) =
  let (now, later) = B.splitAt len chunk
  in (later:xs, now)
