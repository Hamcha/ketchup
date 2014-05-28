{-# LANGUAGE OverloadedStrings #-}

module Ketchup.Httpd
( HTTPRequest
, method, uri, httpver, headers
, listenHTTP
) where

import           Control.Concurrent (forkIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as Map
import           Network
import qualified Network.Socket as NS
import           Network.Socket.ByteString
import           Ketchup.Utils
import           System.IO

-- |HTTP Request type
-- This holds a complete HTTP request
data HTTPRequest = HTTPRequest { method  :: B.ByteString
                                -- ^ Get HTTP Request Method (GET, POST etc.)
                               , uri     :: B.ByteString
                                -- ^ Get Request URI
                               , httpver :: B.ByteString
                                -- ^ Get HTTP Version
                               , headers :: Map.Map B.ByteString [B.ByteString]
                                -- ^ Get HTTP Headers (Header, [value, value])
                               } deriving (Show)

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

-- |Listens for incoming HTTP request
listenHTTP :: String                            -- ^ Address to bind (ie. "*")
           -> PortNumber                        -- ^ Port to listen on
           -> (Socket -> HTTPRequest -> IO ())  -- ^ Route function to call
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
