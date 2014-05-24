{-# LANGUAGE OverloadedStrings #-}

module Ketchup.Chunked
( chunkHeaders
, chunk
, endchunk
) where

import qualified Data.ByteString.Char8 as B
import qualified Data.List as List
import           Ketchup.Httpd
import           Network
import           Network.Socket.ByteString
import           Numeric

chunkHeaders :: Socket -> Int -> [(B.ByteString, [B.ByteString])] -> IO ()
chunkHeaders handle status headers = do
    sendAll handle content
    where
    content = B.concat ["HTTP/1.1 ", statusMsg status, "\r\n\
        \Server: Ketchup\r\n\
        \Connection: keep-alive\r\n",heads,"\r\n\
        \Transfer-Encoding: chunked\r\n\r\n"]
    heads = B.concat $ map (\x -> B.concat [fst x, ": ", B.concat $ List.intersperse "," $ snd x]) headers


chunk :: Socket -> B.ByteString -> IO ()
chunk handle value =
    sendAll handle content
    where
    content = B.concat [B.pack $ showHex (B.length value) "",
                        "\r\n", value, "\r\n"]

endchunk :: Socket -> IO ()
endchunk handle = sendAll handle "0\r\n\r\n"