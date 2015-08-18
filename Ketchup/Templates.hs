{-# LANGUAGE OverloadedStrings #-}

module Ketchup.Templates
( sendTemplate
) where

import qualified Data.ByteString.Char8 as B
import           Ketchup.Routing
import           Ketchup.Utils
import           Ketchup.Static (readFileToString)

sendTemplate :: B.ByteString -> Route
sendTemplate tempFile hnd _ params =
   readFileToString tempFile
    >>= \what ->
        case what of
            Nothing              -> sendNotFound hnd
            Just (content, mime) ->
                sendWithMime hnd mime parsed
                where
                parsed = parseTemplate content params

-- |Parses a template string and replaces {{parameters}} with the corresponding value
parseTemplate :: B.ByteString                         -- ^ The raw template string
              -> (B.ByteString -> Maybe B.ByteString) -- ^ The parameters
              -> B.ByteString
parseTemplate ""  _      = ""
parseTemplate raw params =
    B.concat [r, evalParam params par, parseTemplate rs params]
    where
    -- Isolate template parameters (surrounded by {{ }})
    (r, rest) = breakBS "{{" raw
    (par, rs) = breakBS "}}" rest
    -- Do the substitution
    evalParam :: (B.ByteString -> Maybe B.ByteString) -> B.ByteString -> B.ByteString
    evalParam params par = fallback (params par) ""
