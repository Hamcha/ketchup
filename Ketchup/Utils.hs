{-# LANGUAGE OverloadedStrings #-}

module Ketchup.Utils
( trim
, sendReply
, sendBadRequest
, sendNotFound
, statusMsg
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.Char (isSpace)
import qualified Data.List as List
import           Network
import           Network.Socket.ByteString

-- Returns status message from status id
statusMsg :: Int -> B.ByteString
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
sendBadRequest :: Socket -> IO ()
sendBadRequest client = do
    sendReply client 400 [("Content-Type",["text/plain"])] "400 Bad Request!\n"

sendNotFound :: Socket -> IO ()
sendNotFound client = do
    sendReply client 404 [("Content-Type",["text/plain"])] "404 Not Found!\n"

-- Template for 200 Reply
sendReply :: Socket -> Int -> [(B.ByteString, [B.ByteString])] -> B.ByteString -> IO ()
sendReply client status headers body = do
    sendAll client reply
    where
    reply = B.concat ["HTTP/1.1 ", statusMsg status,"\r\n\
        \Content-Length: ", C.pack $ show $ C.length body, "\r\n\
        \Connection: close\r\n",heads,"\r\n",body]
    -- Turn ("a", ["b", "c"]) headers into "a: b,c"
    heads = B.concat $ map (\x -> B.concat [fst x, ": ", B.concat $ List.intersperse "," $ snd x, "\r\n"]) headers


-- Util function for trimming whitespace from headers
trim :: B.ByteString -> B.ByteString
trim = f . f
    where f = C.reverse . C.dropWhile isSpace
