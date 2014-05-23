{-# LANGUAGE OverloadedStrings #-}

module Ketchup.Routing
( route
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import           Ketchup.Httpd

route []         handle request = sendBadRequest handle
route (r:routes) handle request
    | match (uri request) (fst r) = (snd r) handle request $ params (uri request) (fst r)
    | otherwise                   = route routes handle request

match url template =
    and $ zipWith compare urlparts tmpparts
    where
    compare x y
        | x == y                  = True
        | or [C.null y, C.null x] = False
        | C.head y == ':'         = True
        | otherwise               = False
    urlparts = C.split '/' url
    tmpparts = C.split '/' template

params url template =
    M.fromList $ filter (not . C.null . fst) $ zipWith retrieve urlparts tmpparts
    where
    retrieve x y
        | or [C.null y, C.null x] = ("","")
        | C.head y == ':'         = (C.tail y, x)
        | otherwise               = ("","")
    urlparts = C.split '/' url
    tmpparts = C.split '/' template