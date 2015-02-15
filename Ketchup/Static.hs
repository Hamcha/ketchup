{-# LANGUAGE OverloadedStrings #-}

module Ketchup.Static
( static
) where

import qualified Data.ByteString.Char8 as B
import           Data.Text.Encoding
import           Ketchup.Httpd
import           Ketchup.Utils
import           Network.Mime
import           System.Directory (doesFileExist)

-- |Static file handler
-- Takes a directory and returns a route
static :: B.ByteString -- ^ Path to serve static files from
          -> Handler
static folder hnd req =
    doesFileExist strPath
    >>= \exists ->
        case and [sanecheck path, exists] of
            False -> sendNotFound hnd
            True  -> B.readFile strPath
                     >>= sendReply hnd 200 [("Content-Type",[mime])]
                     where
                     mime = defaultMimeLookup $ decodeUtf8 fname
                     fname = last $ B.split '/' path
    where
    strPath = B.unpack path
    path    = B.concat [folder, uri req]

sanecheck :: B.ByteString -> Bool
sanecheck url =
    and [parentcheck]
    where
    parentcheck = length (filter (== "..") pieces) < 1
    pieces = B.split '/' url
