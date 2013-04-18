{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
-- |
-- Module    : Network.Curl.Aeson
-- Copyright : (c) 2013, Joel Lehtonen
-- License   : BSD3
--
-- Maintainer: Joel Lehtonen <joel.lehtonen+curlaeson@iki.fi>
-- Stability : experimental
-- Portability: portable
--
-- Functions for communicating with JSON over HTTP connection.

module Network.Curl.Aeson
       ( -- * How to use this library
         -- $use
         
         -- * Sending HTTP request
         runHttpJson
         -- * Helper functions
       , cookie
       , rawJson
       , (...)
       , noData
         -- * Exception handling
       , HttpJsonException(..)
       ) where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 (pack,unpack)
import Data.Maybe
import Data.Text (Text)
import Data.Typeable
import Network.Curl

-- | Send single HTTP request.
-- 
-- The request automatically has Content-type: application/json
-- header if you pass any data. This function is lenient on response
-- content type: everything is accepted as long as it is parseable
-- with 'decode'.
-- 
-- If you need authentication, you need to pass session cookie or
-- other means of authentication tokens via 'CurlOption' list.
runHttpJson :: (ToJSON a,FromJSON b)
               => String       -- ^ Request method
               -> URLString    -- ^ Request URL
               -> Maybe a      -- ^ JSON data to send, or Nothing when
                               -- sending request without any content.
               -> [CurlOption] -- ^ Session cookies, or other cURL
                               -- options. Use empty list if you don't
                               -- need any.
               -> IO b         -- ^ Received JSON data
runHttpJson method url maybeContent extraOpts = do
  (curlCode,received) <- curlGetString url curlOpts
  when (curlCode /= CurlOK) $ throw HttpJsonException{errorMsg="HTTP error",..}
  maybe (throw HttpJsonException{errorMsg="JSON parsing has failed",..}) return
    (decode $ pack received)
  where
    curlOpts = commonOpts++dataOpts++extraOpts
    commonOpts = [CurlCustomRequest method]
    dataOpts = case maybeContent of
      Nothing -> []
      Just a  -> [CurlPostFields [unpack $ encode a]
                 ,CurlHttpHeaders ["Content-type: application/json"]
                 ]

-- | Helper function for writing parsers for JSON objects which are
-- not needed to be parsed completely.
--
-- In this example we are parsing JSON from
-- <http://json.org/example.html>.  Note the use of the
-- @OverloadedStrings@ language extension which enables 'Text' values
-- to be written as string literals.
--
-- @p ('Object' o) = 'pure' obj'...'\"glossary\"'...'\"title\"
--p _ = 'mzero'
-- @
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

-- |To avoid ambiguity in type checker you may pass this value instead
-- of Nothing to 'runHttpJson'.
noData :: Maybe Value
noData = Nothing

-- | This exception is is thrown when Curl doesn't finish cleanly or
-- the parsing of JSON response fails.
data HttpJsonException = HttpJsonException { url      :: URLString
                                           , curlCode :: CurlCode
                                           , curlOpts :: [CurlOption]
                                           , received :: String
                                           , errorMsg :: String
                                           } deriving (Show, Typeable)
instance Exception HttpJsonException

-- $use
--
-- To get bid and ask levels from a Bitcoin market:
-- 
-- @{-\# LANGUAGE OverloadedStrings #-}
--import Control.Applicative
--import Control.Monad
--import Data.Aeson
--import Network.Curl.Aeson
--
--data Ticker = Ticker { bid :: 'Double'
--                     , ask :: 'Double'
--                     } deriving ('Show')
--
--instance 'FromJSON' Ticker where
--    parseJSON ('Object' o) = Ticker '<$>' o '.:' \"bid\" '<*>' o '.:' \"ask\"
--    parseJSON _ = 'mzero'
--
--ticker :: IO Ticker
--ticker = 'runHttpJson' \"GET\" \"https:\/\/bitcoin-central.net\/api\/v1\/ticker\/eur\" noData []
-- @
