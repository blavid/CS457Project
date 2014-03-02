{-# LANGUAGE DeriveDataTypeable #-}

import Text.JSON
import Text.JSON.Generic
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString                 as BS
--import Language.Haskell.TH.Ppr (bytesToString)
import Data.Char

-- Trimet returns a JSON response that contains a hierarchical dataset:
-- resultSet
--   LocationList
--   ArrivalList
--   queryTime

-- Data Type Definitions and FromJSON Instance Definitions ---------------------

data ResultSet
     = ResultSet     { location    :: LocationList
                      ,queryTime    :: String
                     } deriving (Show, Data, Typeable)

{-
data ResultSet
     = ResultSet     { locations    :: LocationList
                      ,arrivals     :: ArrivalList
                      ,queryTime    :: String
                     } deriving (Show, Data, Typeable)
-}

data TripList        = TripList     {triplist     :: [Trip]}     deriving (Show, Data, Typeable)

data LocationList    = LocationList {locationList :: [Location]} deriving (Show, Data, Typeable)

data Location
     = Location      { loc_desc           :: String
                      ,loc_locid          :: Int
                      ,loc_dir            :: String
                      ,loc_lng            :: Double
                      ,loc_lat            :: Double
                     } deriving (Show, Data, Typeable)

data ArrivalList     = ArrivalList  {arrivalList  :: [Arrival]}  deriving Show

data Arrival
     = Arrival       { arr_detour         :: Bool
                      ,arr_status         :: String
                      ,arr_locid          :: Int
                      ,arr_block          :: Int
                      ,arr_scheduled      :: String
                      ,arr_shortSign      :: String
                      ,arr_dir            :: Int
                      ,estimated      :: Maybe String
                      ,route          :: Int
                      ,departed       :: Bool
                      ,blockPosition  :: Maybe BlockPosition
                      ,fullSign       :: String
                      ,piece          :: String
                     } deriving (Show, Data, Typeable)

data BlockPosition  
     = BlockPosition { bp_at                 :: String
                      ,bp_feet               :: Int
                      ,bp_lng                :: Double
                      ,bp_trip               :: Trip
                      ,bp_lat                :: Double
                      ,bp_heading            :: Int 
                      } deriving (Show, Data, Typeable)

data Trip           
     = Trip          { trip_progress      :: Int
                      ,trip_desc          :: String
                      ,trip_pattern       :: Int
                      ,trip_dir           :: Int
                      ,trip_route         :: Int
                      ,trip_tripNum       :: Int
                      ,trip_destDist      :: Int
                     } deriving (Show, Data, Typeable)

baseURL :: String
baseURL = "http://developer.trimet.org/ws/V1/arrivals/appID/3B5489BFA2CDF3D5711521B76/json/true/"

stopID  :: String
stopID   = "9843"

arrivalURL :: String -> String
arrivalURL id  = (baseURL ++ "locIDs/" ++ id ++ "/")

getJSON :: String -> IO B.ByteString
getJSON s = simpleHttp (arrivalURL s)

doSomething	    :: Maybe ResultSet -> String
doSomething Nothing  = "Returned Nothing."
doSomething (Just u) = "Returned Something"

--bytesToString :: [Word8] -> String
--bytesToString  = map (chr . fromIntegral)


main :: IO()
main = do 
       json <- getJSON stopID
--       putStrLn (show (decode json :: (Maybe Value)))
       putStrLn (show (decodeJSON (map (chr . fromIntegral) (B.unpack json)) :: ResultSet ))
--       putStrLn (show (decodeJSON (BSC8.unpack json) :: Maybe ResultSet))
--       putStrLn (doSomething (decodeJSON json :: Maybe ResultSet))
