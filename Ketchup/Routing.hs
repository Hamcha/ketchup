{-# LANGUAGE OverloadedStrings #-}

module Ketchup.Routing
( Route      (..)
, Parameters (..)
, route
) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import           Ketchup.Httpd
import           Ketchup.Utils
import           Network
import qualified Text.Regex.PCRE as R

type Parameters = M.Map B.ByteString B.ByteString
type Route = Socket -> HTTPRequest -> Parameters -> IO ()

-- |Router function
-- Takes a list of routes and iterates through them for every requeust
route :: [(B.ByteString, Route)] -- ^ Routes
         -> Handler
route []         handle request = sendNotFound handle
route (r:routes) handle request
    | match (uri request) (fst r) = (snd r) handle request $
                                        params (uri request) (fst r)
    | otherwise                   = route routes handle request

match :: B.ByteString -> B.ByteString -> Bool
match url template =
    and $ zipWith compare urlparts tmpparts
    where
    compare x y
        | x == y                  = True
        | or [B.null y, B.null x] = False
        | B.head y == ':'         = True
        | x R.=~ y                = True
        | otherwise               = False
    urlparts = B.split '/' url
    tmpparts = B.split '/' template

params :: B.ByteString -> B.ByteString -> Parameters
params url template =
    M.fromList $ filter (not . B.null . fst) $
        zipWith retrieve urlparts tmpparts
    where
    retrieve x y
        | or [B.null y, B.null x] = ("","")
        | B.head y == ':'         = (B.tail y, x)
        | otherwise               = ("","")
    urlparts = B.split '/' url
    tmpparts = B.split '/' template
