{-# LANGUAGE OverloadedStrings #-}

module Ketchup.Chunked
( chunkHeaders
, chunk
, endchunk
) where

import qualified Data.ByteString.Char8 as B
import qualified Data.List as List
import           Ketchup.Httpd
import           Ketchup.Utils
import           Network
import           Network.Socket.ByteString
import           Numeric

-- |Send HTTP reply headers and begin a Chunked transmission
chunkHeaders :: Socket                           -- ^ Socket to write to
             -> Int                              -- ^ Status code
             -> [(B.ByteString, [B.ByteString])] -- ^ Headers
             -> IO ()
chunkHeaders handle status headers = do
    sendAll handle content
    where
    content = B.concat ["HTTP/1.1 ", statusMsg status, "\r\n\
        \Connection: close\r\n",heads,"\r\n\
        \Transfer-Encoding: chunked\r\n\r\n"]
    heads = B.concat $ map toHeader headers
    toHeader x = B.concat [fst x, ": "
                          ,B.concat $ List.intersperse "," $ snd x
                          ,"\r\n"]

-- |Sends a chunk
chunk :: Socket       -- ^ Socket to write to
      -> B.ByteString -- ^ Content to write
      -> IO ()
chunk handle value =
    sendAll handle content
    where
    content = B.concat [B.pack $ showHex (B.length value) "",
                        "\r\n", value, "\r\n"]

-- |Send the final/closing chunk
endchunk :: Socket -- ^ Socket to write to
         -> IO ()
endchunk handle = sendAll handle "0\r\n\r\n"
