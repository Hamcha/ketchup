{-# LANGUAGE OverloadedStrings #-}

module Ketchup.Routing
( Route      (..)
, match
, prefix
, route
, useHandler
) where

import qualified Data.ByteString.Char8 as B
import           Ketchup.Httpd
import           Ketchup.Utils
import           Network

type Route   = Socket -> HTTPRequest -> (B.ByteString -> Maybe B.ByteString) -> IO ()
type Matcher = B.ByteString -> (Bool, Arguments)
data Arguments = None | Parameters [(B.ByteString, B.ByteString)]
    deriving Show

-- |Router function
-- Takes a list of routes and iterates through them for every requeust
route :: [(Matcher, Route)] -- ^ Routes
         -> Handler
route []         handle request = sendNotFound handle
route (r:routes) handle request
    | isMatch   = (snd r) handle request (get params)
    | otherwise = route routes handle request
    where
    isMatch = fst matched
    params  = snd matched
    matched = (fst r) (uri request)

-- |Wrap a handler in a route
-- Lets you use a handler (no parameters) as a route
useHandler :: Handler -> Route
useHandler handler hnd req params = handler hnd req

-- |Create a matchable template with parameters (:param)
match :: B.ByteString -> Matcher
match template url =
    (isMatch, Parameters params)
    where
    isMatch  = fst parsed
    params   = snd parsed
    parsed   = parse urlparts temparts []
    urlparts = B.split '/' url
    temparts = B.split '/' template

parse :: [B.ByteString]
      -> [B.ByteString]
      -> [(B.ByteString, B.ByteString)]
      -> (Bool, [(B.ByteString, B.ByteString)])
parse []      []       params = (True, params)
parse [""]    []       params = (True, params)
parse (u:url) []       params = (False, [])
parse []      (t:temp) params = (False, [])
parse (u:url) (t:temp) params
    | B.length t < 1  = parse url temp params
    | B.length u < 1  = parse url (t:temp) params
    | B.head t == ':' = parse url temp ((B.tail t, u) : params)
    | u == t          = parse url temp params
    | otherwise       = (False, [])

-- |Create a prefix matcher
prefix :: B.ByteString -> Matcher
prefix urlPrefix url = (B.isPrefixOf urlPrefix url, None)

get :: Arguments -> B.ByteString -> Maybe B.ByteString
get (Parameters params) x = lookup x params
get None x                = Nothing