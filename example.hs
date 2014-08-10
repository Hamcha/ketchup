{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import           Ketchup.Auth
import           Ketchup.Httpd
import           Ketchup.Routing
import           Ketchup.Utils
import           Ketchup.Chunked
import           Ketchup.Static

handle hnd req =
    sendReply hnd 200 [("Content-Type", ["text/html"])] response
    where
    response = B.concat ["<center>You requested <b>", url, "</b></center>"]
    url = uri req

greet hnd req params =
    sendReply hnd 200 [("Content-Type", ["text/html"])] response
    where
    response = B.concat ["<h1>Hi ", name, "!</h1>"]
    name = fallback (params "user") "Anonymous"

chunked hnd req = do
    chunkHeaders hnd 200 [("Content-Type",["text/plain"])]
    chunk hnd "PUTIFERIO"
    chunk hnd "AAAAAAAAAHHH"
    endchunk hnd

post hnd req = do
    print $ parseBody $ body req
    sendReply hnd 200 [("Content-Type", ["text/html"])] "OK!"

router = route [ ("/greet/:user" , greet                   )
               , ("/chunk/?"     , useHandler $ chunked    )
               , ("/post"        , useHandler $ post       )
               , ("/Ketchup/(.*)", useHandler $ static "." )
               , ("/auth"        , useHandler $ basicAuth [("a","b")] "test" handle )
               , ("/"            , useHandler $ handle     )
               ]

main = do listenHTTP "*" 8080 router
