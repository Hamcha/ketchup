{-# LANGUAGE OverloadedStrings #-}

module Ketchup.Httpd
( HTTPRequest (..)
, Headers
, Handler
, listenHTTP
) where

import           Control.Concurrent (forkIO)
import qualified Data.ByteString.Char8 as B
import           Network
import qualified Network.Socket as NS
import           Network.Socket.ByteString
import           Ketchup.Utils

type Headers  = [(B.ByteString, [B.ByteString])]
type Handler  = Socket -> HTTPRequest -> IO ()

-- |HTTP Request type
-- This holds a complete HTTP request
data HTTPRequest = HTTPRequest
    { method  :: B.ByteString -- ^ Get HTTP Request Method (GET, POST etc.)
    , uri     :: B.ByteString -- ^ Get Request URI
    , httpver :: B.ByteString -- ^ Get HTTP Version
    , headers :: Headers      -- ^ Get HTTP Headers (header, [values])
    , body    :: B.ByteString -- ^ Get HTTP Post body (raw string)
    } deriving (Show)

-- Parses header lines
parseRequestLine :: B.ByteString -> (B.ByteString, [B.ByteString])
parseRequestLine line =
    (property, values)
    where
    property = head items
    values   = B.split ',' $ (trim . last) items
    items    = B.split ':' line

-- Gets all request lines
getRequest :: Socket -> IO ([B.ByteString], B.ByteString)
getRequest client = do
    content <- recv client 1024
    let (reqheaders, reqbody) = breakBS "\r\n\r\n" content
    return (B.lines reqheaders, reqbody)

-- Parses requests
parseRequest :: ([B.ByteString], B.ByteString) -> HTTPRequest
parseRequest reqlines =
    HTTPRequest { method=met, uri=ur, httpver=ver, headers=heads, body=reqbody }
    where
    [met, ur, ver] = B.words $ head reqheaders -- First line is METHOD URI VERSION
    heads   = map parseRequestLine $ tail reqheaders
    (reqheaders, reqbody) = reqlines

-- Handles each client request
handleRequest :: Socket -> Handler -> IO ()
handleRequest client cback = do
    reqlines <- getRequest client
    case length (fst reqlines) of
        0 -> sendBadRequest client
        _ -> cback client $ parseRequest reqlines
    sClose client

-- Acceptor
acceptAll :: Socket -> Handler -> IO ()
acceptAll sock cback = do
    (client, _) <- NS.accept sock
    handleRequest client cback
    acceptAll sock cback

-- Creates Acceptor pools
createAcceptorPool :: Socket -> Int -> Handler -> IO ()
createAcceptorPool sock 0   cback = acceptAll sock cback
createAcceptorPool sock cur cback = do
    forkIO (acceptAll sock cback)
    createAcceptorPool sock (cur-1) cback

-- Gets host from hostname string
getHostaddr :: String -> IO NS.HostAddress
getHostaddr "*"  = return NS.iNADDR_ANY
getHostaddr host = NS.inet_addr host

-- |Listens for incoming HTTP request
listenHTTP :: String        -- ^ Address to bind (ie. "*")
           -> PortNumber    -- ^ Port to listen on
           -> Handler       -- ^ Route function to call
           -> IO ()
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
