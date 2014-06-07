{-# LANGUAGE OverloadedStrings #-}

module Ketchup.Utils
( trim
, breakBS
, parseBody
, sendReply
, sendBadRequest
, sendNotFound
, statusMsg
) where

import qualified Data.ByteString.Char8 as B
import           Data.Char (isSpace)
import qualified Data.List as List
import qualified Data.Map  as Map
import           Network
import           Network.Socket.ByteString

-- |Status Messages
-- Returns status message from a given status id
statusMsg :: Int          -- ^ Status code (ex. 404)
          -> B.ByteString -- ^ Status message (ex. "404 Not Found")
statusMsg stat
    -- 200 Success
    | stat == 200 = "200 OK"
    | stat == 201 = "201 Created"
    | stat == 204 = "204 No Content"
    -- 400 Client Errors
    | stat == 400 = "400 Bad Request"
    | stat == 403 = "403 Forbidden"
    | stat == 404 = "404 Not Found"
    | stat == 405 = "405 Method Not Allowed"
    | stat == 410 = "410 Gone"
    -- 500 Server Errors
    | stat == 500 = "500 Internal Server Error"
    | stat == 501 = "501 Not Implemented"
    | stat == 502 = "502 Bad Gateway"
    | stat == 503 = "503 Service Unavailable"
    | otherwise   = "500 Internal Server Error"

-- Premade HTTP replies

-- |Send 400 Bad Request error
sendBadRequest :: Socket -> IO ()
sendBadRequest client =
    sendReply client 400 [("Content-Type",["text/plain"])] "400 Bad Request!\n"

-- |Send 404 Not Found error
sendNotFound :: Socket -> IO ()
sendNotFound client =
    sendReply client 404 [("Content-Type",["text/plain"])] "404 Not Found!\n"

-- |Send a HTTP reply
-- Sends a reply with the given parameters
sendReply :: Socket                           -- ^ Socket to write to
          -> Int                              -- ^ Status Code to send
          -> [(B.ByteString, [B.ByteString])] -- ^ HTTP headers ("Header",["value1", "value2"])
          -> B.ByteString                     -- ^ Body
          -> IO ()
sendReply client status headers body =
    sendAll client reply
    where
    reply = B.concat
        [ "HTTP/1.1 ", statusMsg status,"\r\n"
        , "Content-Length: ", B.pack $ show $ B.length body, "\r\n"
        , "Connection: close\r\n", heads, "\r\n", body]
    -- Turn ("a", ["b", "c"]) headers into "a: b,c"
    heads = B.concat $ map toHeader headers
    toHeader x = B.concat [fst x, ": "
                          ,B.concat $ List.intersperse "," $ snd x
                          ,"\r\n"]

-- |Trim whitespace from headers
trim :: B.ByteString -> B.ByteString
trim = f . f
    where f = B.reverse . B.dropWhile isSpace

-- |ByteString breakSubstring wrapper that drops delimiters
breakBS :: B.ByteString -> B.ByteString -> (B.ByteString, B.ByteString)
breakBS delimiter source =
    (first, second)
    where
    first  = fst broke
    second = B.drop (B.length delimiter) $ snd broke
    broke  = B.breakSubstring delimiter source

-- |Parse a URL-encoded Request
parseBody :: B.ByteString -> Map.Map B.ByteString B.ByteString
parseBody body = Map.fromList $ map (breakBS "=") $ B.split '&' body