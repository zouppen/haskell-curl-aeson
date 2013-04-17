{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Network.Curl.Aeson where

import Control.Exception
import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 (pack,unpack)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Typeable
import Network.Curl

-- |Send single HTTP request.
runHttpJson :: (ToJSON a,FromJSON b)
               => String       -- ^ Request method
               -> URLString    -- ^ Request URL
               -> Maybe a      -- ^ JSON data to send
               -> [CurlOption] -- ^ Session cookies, or other cURL
                               -- options. Use empty list if you don't
                               -- need any.
               -> IO b         -- ^ Received JSON data
runHttpJson method url maybeContent extraOpts = do
  (curlCode,received) <- curlGetString url curlOpts
  when (curlCode /= CurlOK) $ throw CurlJsonException{errorMsg="HTTP error",..}
  maybe (throw CurlJsonException{errorMsg="JSON parsing has failed",..}) return
    (decode $ pack received)
  where
    curlOpts = commonOpts++dataOpts++extraOpts
    commonOpts = [CurlCustomRequest method]
    dataOpts = case maybeContent of 
      Just a  -> [CurlPostFields [unpack $ encode a]
                 ,CurlHttpHeaders ["Content-type: application/json"]
                 ]
      Nothing -> []

-- | Helper function for writing parsers for JSON objects which are
-- not needed to be parsed completely.
--
-- Parsing example JSON from http://json.org/example.html:
--
-- @p (Object o) = pure obj...\"glossary\"...\"title\"
-- p _ = mzero@
(...) :: FromJSON b
         => Parser Object -- ^ Parser to JSON object to look into
         -> Text          -- ^ Key to look for
         -> Parser b      -- ^ Parser to the resulting field
(...) p s = do
  o <- p
  o .: s

-- Precedence should be higher than >> and >>= but lower than ++
infixl 4 ...

-- | Single cookie of given key and value.
cookie :: String -> String -> CurlOption
cookie key value = CurlCookie $ key++"="++value

-- | Useful for just giving the JSON as string when it is static
-- anyway and doesn't need to be programmatically crafted.
rawJson :: String -> Maybe Value
rawJson = decode . pack

-- Custom exception type for holding HTTP error information
data CurlJsonException = CurlJsonException { url      :: URLString
                                           , curlCode :: CurlCode 
                                           , curlOpts :: [CurlOption]
                                           , received :: String
                                           , errorMsg :: String
                                           } deriving (Show, Typeable)
instance Exception CurlJsonException
