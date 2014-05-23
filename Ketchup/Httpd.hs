{-# LANGUAGE OverloadedStrings #-}

module Ketchup.Httpd
( HTTPRequest
, method, uri, httpver, headers
, statusMsg
, sendReply
, listenHTTP
, sendNotFound, sendBadRequest, sendNotImplemented
) where

import           Control.Concurrent (forkIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as Map
import qualified Data.List as List
import           Network
import qualified Network.Socket as NS
import           Network.Socket.ByteString
import           Ketchup.Utils
import           System.IO

-- HTTP Request type
data HTTPRequest = HTTPRequest { method  :: B.ByteString
                               , uri     :: B.ByteString
                               , httpver :: B.ByteString
                               , headers :: Map.Map B.ByteString [B.ByteString]
                               } deriving (Show)

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

sendBadRequest :: Socket -> IO ()
sendBadRequest client = do
    sendReply client 400 [("Content-Type",["text/plain"])] "Speak proper HTTP ffs!\n"

sendNotFound :: Socket -> IO ()
sendNotFound client = do
    sendReply client 404 [("Content-Type",["text/plain"])] "These aren't your ghosts.\n"

sendNotImplemented :: Socket -> IO ()
sendNotImplemented client = do
    sendReply client 501 [("Content-Type",["text/plain"])] "Oh sorry, management said no to that feature.\n"

-- Sends HTTP reply
sendReply :: Socket -> Int -> [(B.ByteString, [B.ByteString])] -> B.ByteString -> IO ()
sendReply client status headers body = do
    sendAll client reply
    where
    reply = B.concat ["HTTP/1.1 ", statusMsg status,"\r\n\
        \Server: OlegDB/fresh_cuts_n_jams\r\n\
        \Content-Length: ", C.pack $ show $ C.length body, "\r\n\
        \Connection: close\r\n",heads,"\r\n",body]
    -- Turn ("a", ["b", "c"]) headers into "a: b,c"
    heads = B.concat $ map (\x -> B.concat [fst x, ": ", B.concat $ List.intersperse "," $ snd x, "\r\n"]) headers

-- Parses header lines
parseRequestLine :: B.ByteString -> (B.ByteString, [B.ByteString])
parseRequestLine line =
    (property, values)
    where
    property = head items
    values = C.split ',' $ (trim . last) items
    items = C.split ':' line

-- Gets all request lines
getRequest :: Socket -> IO [B.ByteString]
getRequest client = do
    content <- recv client 1024
    return $ C.lines content

-- Parses requests
parseRequest :: [B.ByteString] -> HTTPRequest
parseRequest reqlines =
    HTTPRequest { method=met, uri=ur, httpver=ver, headers=heads }
    where
    [met, ur, ver] = (C.words . head) reqlines -- First line is METHOD URI VERSION
    heads = Map.fromList $ map parseRequestLine $ tail reqlines

-- Handles each client request
handleRequest :: Socket -> (Socket -> HTTPRequest -> IO ()) -> IO ()
handleRequest client cback = do
    reqlines <- getRequest client
    case length reqlines of
        0 -> sendBadRequest client
        _ -> cback client $ parseRequest reqlines
    sClose client

-- Acceptor
acceptAll :: Socket -> (Socket -> HTTPRequest -> IO ()) -> IO ()
acceptAll sock cback = do
    (client, _) <- NS.accept sock
    handleRequest client cback
    acceptAll sock cback

-- Creates Acceptor pools
createAcceptorPool :: Socket -> Int -> (Socket -> HTTPRequest -> IO ()) -> IO ()
createAcceptorPool sock max cback =
    case max of
        0 -> acceptAll sock cback
        x -> do
            forkIO $ acceptAll sock cback
            createAcceptorPool sock (x-1) cback

-- Gets host from hostname string
getHostaddr :: String -> IO NS.HostAddress
getHostaddr "*"  = return NS.iNADDR_ANY
getHostaddr host = NS.inet_addr host

-- Listens for incoming HTTP request
listenHTTP :: String -> PortNumber -> (Socket -> HTTPRequest -> IO ()) -> IO ()
listenHTTP hostname port cback = withSocketsDo $ do
    -- Get hostname
    host <- getHostaddr hostname
    let addr = NS.SockAddrInet port host
    -- Prepare Socket
    sock <- NS.socket NS.AF_INET NS.Stream 0
    NS.setSocketOption sock NS.ReuseAddr 1
    NS.setSocketOption sock NS.NoDelay 1
    -- Bind socket to address and listen
    NS.bindSocket sock addr
    NS.listen sock 128
    createAcceptorPool sock 128 cback
    sClose sock
