<!-- -*- mode: markdown; coding: utf-8 -*- -->

# curl-aeson library for Haskell

This is a library for communicating with JSON over HTTP connection.
It supports rich set of HTTP connectivity features provided by
[curl](https://github.com/galoisinc/curl) library combined to the
performance and elegancy of [aeson](https://github.com/bos/aeson).

This library is at its best when communicating with simple,
non-standardized JSON interfaces. If you are implementing JSON-RPC
compliant client or server, take a look at
[another library](http://hackage.haskell.org/package/jmacro-rpc).

## Example

In this example we fetch latest bid and ask values from a Bitcoin
exchange using
[their public API](https://github.com/paytunia/api-documentation#read-the-ticker):

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Control.Monad
import Data.Aeson
import Network.Curl.Aeson

data Ticker = Ticker { bid :: Double
                     , ask :: Double
                     } deriving (Show)

instance FromJSON Ticker where
    parseJSON (Object o) = Ticker <$> o .: "bid" <*> o .: "ask"
    parseJSON _ = mzero

ticker :: IO Ticker
ticker = runHttpJson "GET" "https://bitcoin-central.net/api/v1/ticker/eur" noData []
```

## Installation

### On Ubuntu and Debian

Starting from Ubuntu 12.04 and Debian wheezy, all the dependencies are
found from the repositories:

    sudo apt-get install libghc-aeson-dev libghc-curl-dev cabal-install

Then just install this:

    cabal install

### Other

Install and configure
[Haskell Platform](http://www.haskell.org/platform/). Then, fetch all the
requirements and install this library by running:

    cabal update
	cabal install
