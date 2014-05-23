{-# LANGUAGE OverloadedStrings #-}

module Ketchup.Utils
( trim
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.Char (isSpace)

-- Util function for trimming whitespace from headers
trim :: B.ByteString -> B.ByteString
trim = f . f
    where f = C.reverse . C.dropWhile isSpace