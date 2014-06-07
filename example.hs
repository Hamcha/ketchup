{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import qualified Data.Map as M
import           Ketchup.Httpd
import           Ketchup.Routing
import           Ketchup.Utils
import           Ketchup.Chunked
import           Ketchup.Static

handle hnd req params =
    sendReply hnd 200 [("Content-Type", ["text/html"])] response
    where
    response = B.concat ["<center>You requested <b>", url, "</b></center>"]
    url = uri req

greet hnd req params =
    sendReply hnd 200 [("Content-Type", ["text/html"])] response
    where
    response = B.concat ["<h1>Hi ", getName name, "!</h1>"]
    getName (Just x) = x
    getName Nothing  = "Anonymous"
    name = M.lookup "user" params

chunked hnd req params = do
    chunkHeaders hnd 200 [("Content-Type",["text/plain"])]
    chunk hnd "PUTIFERIO"
    chunk hnd "AAAAAAAAAHHH"
    endchunk hnd

post hnd req params = do
    print $ parseBody $ body req
    sendReply hnd 200 [("Content-Type", ["text/html"])] "OK!"

router = route [ ("/"            , handle     )
               , ("/greet/:user" , greet      )
               , ("/chunk"       , chunked    )
               , ("/Ketchup/(.*)", static "." )
               , ("/post"        , post       )
               ]

main = do listenHTTP "*" 8080 router
