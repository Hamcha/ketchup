{-# LANGUAGE OverloadedStrings #-}

module Ketchup.Utils
( breakBS
, fallback
, parseBody
, sendBadRequest
, sendNotFound
, sendReply
, sendWithMime
, statusMsg
, subBS
, trim
) where

import qualified Data.ByteString.Char8 as B
import           Data.Char (isSpace)
import qualified Data.List as List
import           Network
import           Network.Socket.ByteString

-- |Status Messages
-- Returns status message from a given status id
statusMsg :: Int          -- ^ Status code (ex. 404)
          -> B.ByteString -- ^ Status message (ex. "404 Not Found")
-- 200 Success
statusMsg 200 = "200 OK"
statusMsg 201 = "201 Created"
statusMsg 204 = "204 No Content"
-- 400 Client Errors
statusMsg 400 = "400 Bad Request"
statusMsg 401 = "401 Unauthorized"
statusMsg 402 = "402 Payment Required"
statusMsg 403 = "403 Forbidden"
statusMsg 404 = "404 Not Found"
statusMsg 405 = "405 Method Not Allowed"
statusMsg 410 = "410 Gone"
-- 500 Server Errors
statusMsg 500 = "500 Internal Server Error"
statusMsg 501 = "501 Not Implemented"
statusMsg 502 = "502 Bad Gateway"
statusMsg 503 = "503 Service Unavailable"
statusMsg _   = "500 Internal Server Error"

-- |Send 400 Bad Request error
sendBadRequest :: Socket -> IO ()
sendBadRequest client =
    sendReply client 400 [("Content-Type",["text/plain"])] "400 Bad Request\n"

-- |Send 404 Not Found error
sendNotFound :: Socket -> IO ()
sendNotFound client =
    sendReply client 404 [("Content-Type",["text/plain"])] "404 Not Found\n"

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
    reply = B.concat [ "HTTP/1.1 ", statusMsg status,"\r\n"
                     , "Content-Length: ", B.pack $ show $ B.length body, "\r\n"
                     , "Connection: close\r\n", heads, "\r\n", body]
    -- Turn ("a", ["b", "c"]) headers into "a: b,c"
    heads      = B.concat $ map toHeader headers
    toHeader x = B.concat [ fst x, ": "
                          , B.concat $ List.intersperse "," $ snd x
                          , "\r\n"]

-- |Trim whitespace from headers
trim :: B.ByteString -> B.ByteString
trim = f . f where f = B.reverse . B.dropWhile isSpace

-- |ByteString breakSubstring wrapper that drops delimiters
breakBS :: B.ByteString -> B.ByteString -> (B.ByteString, B.ByteString)
breakBS delimiter source =
    (first, second)
    where
    first  = fst broke
    second = B.drop (B.length delimiter) $ snd broke
    broke  = B.breakSubstring delimiter source

-- |Get a substring of a ByteString
subBS :: Int -> Int -> B.ByteString -> B.ByteString
subBS start len = B.take len . B.drop start

-- |Parse a URL-encoded Request
parseBody :: B.ByteString -> [(B.ByteString, B.ByteString)]
parseBody body = map (breakBS "=") $ B.split '&' body

-- |Maybe a -> a with fallback (if Nothing)
fallback :: Maybe a -> a -> a
fallback (Just x) _   = x
fallback Nothing  def = def

-- |Send an OK reply with given mime and content
sendWithMime :: Socket -> B.ByteString -> B.ByteString -> IO ()
sendWithMime hnd mime = sendReply hnd 200 [("Content-Type", [mime])]
