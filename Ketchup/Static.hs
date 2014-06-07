{-# LANGUAGE OverloadedStrings #-}

module Ketchup.Static
( static
) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import           Data.Text.Encoding
import           Ketchup.Httpd
import           Ketchup.Routing
import           Ketchup.Utils
import           Network
import           Network.Mime
import           System.Directory (doesFileExist)

-- |Static file handler
-- Takes a directory and returns a route
static :: B.ByteString -- ^ Path to serve static files from
          -> Route
static folder hnd req params = do
    let path = B.concat [folder, uri req]
    let sane = sanecheck path
    let strPath = B.unpack path
    doesExist <- doesFileExist strPath
    case and [sane, doesExist] of
        True  -> B.readFile strPath
                 >>= sendReply hnd 200 [("Content-Type",[mime])]
                 where
                 mime = defaultMimeLookup $ decodeUtf8 fname
                 fname = last $ B.split '/' path
        False -> sendNotFound hnd

sanecheck :: B.ByteString -> Bool
sanecheck url =
    and checks
    where
    checks = [parentcheck]
    parentcheck = length (filter (== "..") pieces) < 1
    pieces = B.split '/' url
