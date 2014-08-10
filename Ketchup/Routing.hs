{-# LANGUAGE OverloadedStrings #-}

module Ketchup.Routing
( Route      (..)
, route
, useHandler
) where

import qualified Data.ByteString as B
import           GHC.Arr
import           Ketchup.Httpd
import           Ketchup.Utils
import           Network
import qualified Text.Regex.TDFA as R

type Route = Socket -> HTTPRequest -> (B.ByteString -> Maybe B.ByteString) -> IO ()

-- |Router function
-- Takes a list of routes and iterates through them for every requeust
route :: [(B.ByteString, Route)] -- ^ Routes
         -> Handler
route []         handle request = sendNotFound handle
route (r:routes) handle request
    | isMatch   = (snd r) handle request params
    | otherwise = route routes handle request
    where
    isMatch = fst matched
    params  = snd matched
    matched = match (uri request) (fst r)


-- |Wrap a handler in a route
-- Lets you use a handler (no parameters) as a route
useHandler :: Handler -> Route
useHandler handler hnd req params = handler hnd req

match :: B.ByteString -> B.ByteString -> (Bool, B.ByteString -> Maybe B.ByteString)
match url template =
    (matched, params)
    where
    matched     = (length paramValues) == (length paramNames)
    params name = lookup name paramList
    paramList   = zipWith (\x y -> (x, y)) paramNames paramValues
    paramValues = map (\x -> subBS (fst x) (snd x) url) $ elems paramRegexp
    paramRegexp = url R.=~ regex :: R.MatchArray
    (regex, paramNames) = prepare template

prepare :: B.ByteString -> (B.ByteString, [B.ByteString])
prepare url =
    (regex, paramNames)
    where
    regex        = B.concat ["^", regexPart, "$"]
    regexPart    = foldr insertBS url $ elems matches
    paramNames   = map (\x -> subBS (fst x) (snd x) url) $ elems matches
    matches      = url R.=~ (":([^/]+)" :: B.ByteString) :: R.MatchArray
    insertBS x a = B.concat [ B.take (fst x) a
                            , "([^/]+)"
                            , B.drop ((fst x) + (snd x)) a]