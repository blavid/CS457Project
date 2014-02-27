{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module TrimetParseJSON where

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

getJSON :: String -> IO B.ByteString
getJSON s = simpleHttp (arrivalURL s)

-- main :: IO()
-- main = do json <- getJSON
--           putStrLn (show (decode json :: (Maybe Value)))
          

-- Define a datatype for a bus line/route
data Arrival =
  Arrival {	detour	:: !Bool,
		status	:: !Text,
		locid	:: !Int,
		block	:: !Int,
		scheduled :: !Text,
		shortSign :: !Text,
		dir	:: !Int,
		estimated :: !Text,
		route	:: !Int,
		departed	:: !Bool
 	  } deriving (Show,Generic)

--data Location = 
--  Location { desc 	:: !Text,
--	     locid	:: !Int,
--	     dir	:: !Text,
--	     lng	:: !Double,
--	     lat	:: !Double
--	   } deriving (Show,Generic)

instance FromJSON Arrival
instance ToJSON Arrival

--main :: IO ()
--main  = do
  -- This is my code below: =================
--  getJSON

  -- This is my code above: =================
  -- Get JSON data and decode it.
--  d <- (eitherDecode <$> getJSON) :: IO (Either String [Arrival])
  -- If d is Left, the JSON was malformed.
  -- In that case, report the error.
  -- Otherwise, use the JSON data.
--  case d of
--    Left err -> putStrLn err
--    Right ps -> print ps
