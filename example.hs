{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Ketchup.Httpd
import qualified Data.ByteString as B

handle hnd req = do
    sendReply hnd 200 [("Content-Type",["text/html"])] response
    where
    response = B.concat ["<center>You requested <b>",url,"</b></center>"]
    url = uri req

main = do listenHTTP "*" 8080 handle