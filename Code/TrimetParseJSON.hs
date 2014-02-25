{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.Stream
-- import Network.HTTP.Conduit (simpleHttp)
import Network.HTTP
import GHC.Generics

baseURL :: String
baseURL = "http://developer.trimet.org/ws/V1/arrivals/appID/3B5489BFA2CDF3D5711521B76/json/true/"

arrivalURL :: String -> String
arrivalURL id  = (baseURL ++ "locIDs/" ++ id ++ "/")

--getJSON :: IO B.ByteString

getJSON :: IO (Network.Stream.Result (Response String))
getJSON = simpleHTTP (getRequest (arrivalURL "9843"))

-- Define a datatype for a bus line/route

data Arrival =
  Arrival { shortSign :: !Text,
	    scheduled :: !Text,
	    estimated :: !Text
 	  } deriving (Show,Generic)

instance FromJSON Arrival
instance ToJSON Arrival

