{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import qualified Data.Map as M
import           Ketchup.Httpd
import           Ketchup.Routing

handle hnd req params = do
    sendReply hnd 200 [("Content-Type", ["text/html"])] response
    where
    response = B.concat ["<center>You requested <b>", url, "</b></center>"]
    url = uri req

greet hnd req params = do
    sendReply hnd 200 [("Content-Type", ["text/html"])] response
    where
    response = B.concat ["<h1>Hi ", getName name, "!</h1>"]
    getName (Just x) = x
    getName Nothing  = "Anonymous"
    name = M.lookup "user" params

router = route [("/", handle), ("/greet/:user", greet)]

main = do listenHTTP "*" 8080 router