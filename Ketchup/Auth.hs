{-# LANGUAGE OverloadedStrings #-}

module Ketchup.Auth
( basicAuth
) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base64 as B64
import           Ketchup.Httpd
import           Ketchup.Utils

-- |Performs HTTP Basic Auth
basicAuth :: [(B.ByteString, B.ByteString)] -- ^ List of (Username, Password)
          -> B.ByteString                   -- ^ Authentication Realm
          -> Handler                        -- ^ Success Handler
          -> Handler
basicAuth couples realm success hnd req =
    case authHead of
        Nothing -> send401
        Just x  -> case authData `elem` couples of
                       False -> send401
                       True  -> success hnd req
                   where
                   authData = parseAuth $ x !! 0
    where
    authHead = lookup "Authorization" $ headers req
    authField = B.concat ["Basic realm=\"",realm,"\""]
    send401 = sendReply hnd 401 [("WWW-Authenticate", [authField])
                                ,("Content-Type", ["text/html"])]
                                "<h1>401 Unauthorized</h1>"

parseAuth :: B.ByteString -> (B.ByteString, B.ByteString)
parseAuth authStr =
    breakBS ":" $ B64.decodeLenient authData
    where
    authData = snd parts
    parts = breakBS " " authStr