{-# LANGUAGE OverloadedStrings #-}

module Ketchup.Routing
( route
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import           Ketchup.Httpd
import           Ketchup.Utils
import           Network
import qualified Text.Regex.Posix as R

route :: [(C.ByteString, (Socket -> HTTPRequest 
         -> (M.Map C.ByteString C.ByteString) -> IO ()))]
         -> (Socket -> HTTPRequest -> IO ())
route []         handle request = sendNotFound handle
route (r:routes) handle request
    | match (uri request) (fst r) = (snd r) handle request $ 
                                        params (uri request) (fst r)
    | otherwise                   = route routes handle request

match :: C.ByteString -> C.ByteString -> Bool
match url template =
    and $ zipWith compare urlparts tmpparts
    where
    compare x y
        | x == y                  = True
        | or [C.null y, C.null x] = False
        | C.head y == ':'         = True
        | x R.=~ y                = True
        | otherwise               = False
    urlparts = C.split '/' url
    tmpparts = C.split '/' template

params :: C.ByteString -> C.ByteString -> M.Map C.ByteString C.ByteString
params url template =
    M.fromList $ filter (not . C.null . fst) $ 
        zipWith retrieve urlparts tmpparts
    where
    retrieve x y
        | or [C.null y, C.null x] = ("","")
        | C.head y == ':'         = (C.tail y, x)
        | otherwise               = ("","")
    urlparts = C.split '/' url
    tmpparts = C.split '/' template
