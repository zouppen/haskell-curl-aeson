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
         curlAesonGet
       , curlAesonGetWith
       , curlAeson
         -- * Helper functions
       , cookie
       , rawJson
       , (...)
       , noData
         -- * Exception handling
       , CurlAesonException(..)
       ) where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.UTF8 (fromString,toString)
import Data.Maybe
import Data.Text (Text)
import Data.Typeable
import Network.Curl

-- | Shorthand for doing just a HTTP GET request and parsing the output to
-- any FromJSON instance.
curlAesonGet :: (FromJSON a) => URLString -> IO a
curlAesonGet = curlAesonGetWith parseJSON

-- | Shorthand for doing just a HTTP GET request and parsing the
-- output with given parser /p/.
curlAesonGetWith :: (Value -> Parser a) -> URLString -> IO a
curlAesonGetWith p url = curlAeson p "GET" url [] noData

-- | Send single HTTP request.
-- 
-- The request automatically has @Content-type: application/json@
-- header if you pass any data. This function is lenient on response
-- content type: everything is accepted as long as it is parseable
-- with 'decode'. HTTP payload is expected to be UTF-8 encoded.
-- 
-- If you need authentication, you need to pass session cookie or
-- other means of authentication tokens via 'CurlOption' list.
curlAeson ::
  (ToJSON a)
  => (Value -> Parser b) -- ^ Parser for response. Use 'parseJSON' if
                         -- you like want to use FromJSON instance or
                         -- 'pure' if you want it in AST format.
  -> String              -- ^ Request method
  -> URLString           -- ^ Request URL
  -> [CurlOption]        -- ^ Session cookies, or other cURL
                         -- options. Use empty list if you don't need
                         -- any.
  -> Maybe a             -- ^ JSON data to send, or Nothing when
                         -- sending request without any content.
  -> IO b                -- ^ Received JSON data
curlAeson parser method url extraOpts maybeContent = do
  (curlCode,received) <- curlGetString url curlOpts
  when (curlCode /= CurlOK) $ throw CurlAesonException{errorMsg="HTTP error",..}
  let ast = case decode $ fromString received of
        Nothing -> throw CurlAesonException{errorMsg="JSON parsing failed",..}
        Just x  -> x
  return $ case parseEither parser ast of
    Left errorMsg -> throw CurlAesonException{..}
    Right x -> x
  where
    curlOpts = commonOpts++dataOpts++extraOpts
    commonOpts = [CurlCustomRequest method]
    dataOpts = case maybeContent of
      Nothing -> []
      Just a  -> [CurlPostFields [toString $ encode a]
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
-- @p ('Object' o) = 'pure' o'...'\"glossary\"'...'\"title\"
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
rawJson = decode . fromString

-- |To avoid ambiguity in type checker you may pass this value instead
-- of Nothing to 'curlAeson'.
noData :: Maybe Value
noData = Nothing

-- | This exception is is thrown when Curl doesn't finish cleanly or
-- the parsing of JSON response fails.
data CurlAesonException = CurlAesonException { url      :: URLString
                                             , curlCode :: CurlCode
                                             , curlOpts :: [CurlOption]
                                             , received :: String
                                             , errorMsg :: String
                                             } deriving (Show, Typeable)
instance Exception CurlAesonException

-- $use
--
-- To get bid and ask levels as a pair from a Bitcoin exchange using its public
-- API:
--
-- @{-\# LANGUAGE OverloadedStrings #-}
--import Control.Monad
--import Data.Aeson
--import Network.Curl.Aeson
--
--ticker :: 'IO' ('Double','Double')
--ticker = 'curlAesonGetWith' p \"https:\/\/bitcoin-central.net\/api\/v1\/ticker\/eur\"
--  where
--    p ('Object' o) = do
--      bid <- o '.:' \"bid\"
--      ask <- o '.:' \"ask\"
--      'return' (bid,ask)
--    p _ = 'mzero'
-- @
-- 
-- The same as above, but we define our own data type which is an
-- instance of FromJSON:
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
--ticker :: 'IO' Ticker
--ticker = 'curlAesonGet' \"https:\/\/bitcoin-central.net\/api\/v1\/ticker\/eur\"
-- @
