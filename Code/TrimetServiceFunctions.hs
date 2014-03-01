{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module TrimetServiceFunctions where

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics

baseURL :: String
baseURL = "http://developer.trimet.org/ws/V1/arrivals/appID/3B5489BFA2CDF3D5711521B76/json/true/"

arrivalURL :: String -> String
arrivalURL id  = (baseURL ++ "locIDs/" ++ id ++ "/")

getArrivals  :: String -> IO B.ByteString
getArrivals s = simpleHttp (arrivalURL s)
