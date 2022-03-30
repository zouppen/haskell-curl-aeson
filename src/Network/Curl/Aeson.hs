{-# LANGUAGE RecordWildCards #-}
-- |
-- Module    : Network.Curl.Aeson
-- Copyright : (c) 2013-2022, Joel Lehtonen
-- License   : BSD3
--
-- Maintainer: Joel Lehtonen <joel.lehtonen+curlaeson@iki.fi>
-- Stability : experimental
-- Portability: portable
--
-- Functions for communicating with JSON over HTTP, HTTPS, or any
-- protocol supported by [cURL](https://curl.se/).

module Network.Curl.Aeson
       ( -- * How to use this library
         -- $use
         
         -- * cURL requests with JSON payload and response
         curlAesonGet
       , curlAesonGetWith
       , curlAesonCustom
       , curlAesonCustomWith
         -- * Generic cURL request
       , curlAesonRaw
         -- * Helpers for working with raw requests
       , jsonPayload
       , binaryPayload
       , binaryResponse
       , valueResponse
       , jsonResponse
         -- * Other helper functions
       , cookie
       , rawJson
       , (...)
       , noData
         -- * Types
       , Payload(..)
       , ResponseParser
         -- * Exception handling
       , CurlAesonException(..)
         -- * Deprecated functions
       , curlAeson
       ) where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Data.Text (Text)
import Data.Typeable
import Network.Curl
import Network.Curl.Aeson.Internal

-- | Shorthand for doing just a HTTP GET request and parsing the output to
-- any 'FromJSON' instance.
curlAesonGet :: (FromJSON a)
  => URLString -- ^ Request URL
  -> IO a      -- ^ Received and parsed data
curlAesonGet url = curlAesonCustom "GET" url [] noData

-- | Shorthand for doing just a HTTP GET request and parsing the
-- output with given parser /p/.
curlAesonGetWith
  :: (Value -> Parser a) -- ^Aeson parser for response. Use 'pure' if
                         -- you want it in AST format.
  -> URLString           -- ^Request URL
  -> IO a                -- ^Received and parsed data
curlAesonGetWith p url = curlAesonCustomWith p "GET" url [] noData

-- | Send a single HTTP request and a custom parser.
-- 
-- The request automatically has @Content-type: application/json@
-- header if you pass any data. This function is lenient on response
-- content type; everything is accepted as long as it is valid JSON
-- and parseable with your supplied parser.
-- 
-- If you need authentication, you need to pass session cookie or
-- other means of authentication tokens via 'CurlOption' list.
curlAesonCustomWith
  :: (ToJSON a)
  => (Value -> Parser b) -- ^ Aeson parser for response. Use
                         -- 'pure' if you want it in AST format.
  -> String              -- ^ Request method
  -> URLString           -- ^ Request URL
  -> [CurlOption]        -- ^ Session cookies, or other cURL
                         -- options. Use 'mempty' if you don't need
                         -- any.
  -> Maybe a             -- ^ JSON data to send, or 'Nothing' when
                         -- sending request without any content.
  -> IO b                -- ^ Received and parsed data
curlAesonCustomWith parser method url extraOpts maybeValue =
  curlAesonRaw method url extraOpts
  (maybeValue >>= jsonPayload)
  (\x -> eitherDecode x >>= parseEither parser)

{-# DEPRECATED curlAeson "Use customAesonCustomWith instead" #-}
-- |See type of 'curlAesonCustomWith'.
curlAeson :: ToJSON a => (Value -> Parser b) -> String -> URLString -> [CurlOption] -> Maybe a -> IO b
curlAeson = curlAesonCustomWith

-- | Send a single cURL request.
--
-- The request automatically has @Content-type: application/json@
-- header if you pass any data. This function is lenient on response
-- content type; everything is accepted as long as 'parseJSON'
-- succeeds.
--
-- If you need authentication, you need to pass session cookie or
-- other means of authentication tokens via 'CurlOption' list.
curlAesonCustom ::
  (ToJSON a, FromJSON b)
  => String              -- ^ Request method
  -> URLString           -- ^ Request URL
  -> [CurlOption]        -- ^ Session cookies, or other cURL
                         -- options. Use 'mempty' if you don't need
                         -- any.
  -> Maybe a             -- ^ JSON data to send, or 'Nothing' when
                         -- sending request without any content.
  -> IO b                -- ^ Received and parsed data
curlAesonCustom method url extraOpts maybeValue =
  curlAesonRaw method url extraOpts
  (maybeValue >>= jsonPayload)
  eitherDecode

-- |Sends raw cURL request with a possible payload and collects the
-- output. When /payload/ is given, cURL options CurlReadFunction and
-- CurlUpload are set and HTTP headers Content-Length and Content-Type
-- are appended.
curlAesonRaw
  :: String              -- ^ Request method
  -> URLString           -- ^ Request URL
  -> [CurlOption]        -- ^ Extra curl options.
  -> Maybe Payload       -- ^ Request body payload, if any.
  -> ResponseParser a    -- ^ Parser function for the response such as 'eitherDecode'
  -> IO a                -- ^ Received and parsed data
curlAesonRaw method url userOpts maybePayload parser = do
  -- Prepare the upload
  putOpts <- case maybePayload of
    Nothing -> pure userOpts
    Just Payload{..} -> do
      readFunc <- mkReadFunctionLazy payload
      pure $ CurlReadFunction readFunc : CurlUpload True : mergeHeaders
        [ "Content-Length: " <> show (B.length payload)
        , "Content-Type: " <> contentType
        ] userOpts
  -- Add request method
  let curlOpts = CurlCustomRequest method : putOpts
  -- Perform the request
  (curlCode, received) <- curlGetString_ url curlOpts
  when (curlCode /= CurlOK) $
    throwIO CurlAesonException{parseError = Nothing, ..}
  -- Trying to parse
  case parser received of
    Left e  -> throwIO CurlAesonException{parseError = Just e, ..}
    Right x -> pure x

-- |Internal tool to merge headers in a cURL option list
mergeHeaders :: [String] -> [CurlOption] -> [CurlOption]
mergeHeaders acc [] = [CurlHttpHeaders acc]
mergeHeaders acc ((CurlHttpHeaders x):xs) = mergeHeaders (acc <> x) xs
mergeHeaders acc (x:xs) = x:mergeHeaders acc xs 

-- |Convert a value to JSON payload. This never returns Nothing.
jsonPayload :: ToJSON a => a -> Maybe Payload
jsonPayload a = Just Payload{..}
  where payload = encode a
        contentType = "application/json"

-- |Just a shortcut for defining binary payloads of given media
-- type. This never returns Nothing.
binaryPayload :: String        -- ^Media type (MIME)
              -> ByteString    -- ^Data
              -> Maybe Payload -- ^Payload
binaryPayload a b = Just $ Payload a b

-- | Helper function for writing parsers for JSON objects which are
-- not needed to be parsed completely.
--
-- In this example we are parsing JSON from
-- <http://json.org/example.html>.  Note the use of the
-- @OverloadedStrings@ language extension which enables 'Text' values
-- to be written as string literals.
--
-- @p ('Data.Aeson.Types.Internal.Object' o) = 'pure' o'...'\"glossary\"'...'\"title\"
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
cookie key value = CurlCookie $ key <> "=" <> value

-- | Useful for just giving the JSON as string when it is static
-- anyway and doesn't need to be programmatically crafted.
rawJson :: ByteString -> Maybe Value
rawJson = decode

-- |Useful with 'curlAesonRaw' when you just need to take the binary output.
binaryResponse :: ResponseParser ByteString
binaryResponse = Right

-- |Useful with 'curlAesonRaw' when you just want to get the JSON Value
valueResponse :: ResponseParser Value
valueResponse = eitherDecode

-- |Just a friendly name for 'eitherDecode'. Can be used with
-- 'curlAesonRaw' for deserializing the response using 'FromJSON'
-- instance.
jsonResponse :: FromJSON a => ResponseParser a
jsonResponse = eitherDecode

-- |To avoid type ambiguity you may pass this value instead
-- of Nothing to 'curlAesonCustom'.
noData :: Maybe Value
noData = Nothing

-- |Holds the payload for raw sender.
data Payload = Payload
  { contentType :: String      -- ^Content MIME type
  , payload     :: ByteString  -- ^Data
  } deriving (Show)

-- | This exception is is thrown when Curl doesn't finish cleanly or
-- the parsing of JSON response fails.
data CurlAesonException = CurlAesonException
  { url        :: URLString    -- ^The request URI
  , curlCode   :: CurlCode     -- ^Curl return code
  , curlOpts   :: [CurlOption] -- ^Curl options set
  , received   :: ByteString   -- ^Received raw data from the
                               -- server. Before version 0.1 the type
                               -- was 'Prelude.String'.
  , parseError :: Maybe String -- ^Parse error, if it failed during parse.
  } deriving (Show)

instance Exception CurlAesonException

-- |Parser type from response to your data. Normally: 'eitherDecode'
type ResponseParser a = ByteString -> Either String a

-- $use
--
-- Let\'s simulate a service by creating a file @\/tmp\/ticker.json@
-- with the following content:
--
-- > {"bid":3,"ask":3.14}
--
-- This example shows how to hand-craft the parser for the bid and ask
-- values:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Control.Monad
-- > import Data.Aeson
-- > import Network.Curl.Aeson
-- >
-- > ticker :: IO (Double,Double)
-- > ticker = curlAesonGetWith p "file:///tmp/ticker.json"
-- >   where
-- >     p (Object o) = do
-- >       bid <- o .: "bid"
-- >       ask <- o .: "ask"
-- >       return (bid,ask)
-- >     p _ = mzero
--
-- The same as above, but we define our own data type which is an
-- instance of 'FromJSON':
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- > import GHC.Generics
-- > import Data.Aeson
-- > import Network.Curl.Aeson
-- >
-- > data Ticker = Ticker { bid :: Double
-- >                      , ask :: Double
-- >                      } deriving (Generic, Show)
-- >
-- > instance FromJSON Ticker
-- >
-- > ticker :: IO Ticker
-- > ticker = curlAesonGet "file:///tmp/ticker.json"
