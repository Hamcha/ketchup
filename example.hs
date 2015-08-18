{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import           Ketchup.Auth
import           Ketchup.Httpd
import           Ketchup.Routing
import           Ketchup.Utils
import           Ketchup.Chunked
import           Ketchup.Static
import           Ketchup.Templates

handle hnd req =
    sendWithMime hnd "text/html" response
    where
    response = B.concat ["<center>You requested <b>", url, "</b></center>"]
    url = uri req

greet hnd _ params =
    sendWithMime hnd "text/html" response
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
    sendWithMime hnd "text/html" "OK!"

router = route [ (match  "/greet/:user"            , greet                   )
               , (match  "/temp/poem/:author/:item", sendTemplate "./greet.html")
               , (prefix "/chunk/"                 , useHandler $ chunked    )
               , (match  "/post"                   , useHandler $ post       )
               , (prefix "/Ketchup/"               , useHandler $ static "." )
               , (match  "/auth"                   , useHandler $ basicAuth [("a","b")] "test" handle )
               , (match  "/"                       , useHandler $ handle     )
               ]

host = "*"
port = 8080

main = do putStrLn $ "Listening on " ++ host ++ ":" ++ (show port)
          listenHTTP host port router

