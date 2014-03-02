{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Aeson
import Control.Applicative
import Data.Text
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics
import Foreign.Marshal.Unsafe
import System.IO.Unsafe

data TripList        = TripList     {triplist     :: [Trip]}     deriving Show
data LocationList    = LocationList {locationList :: [Location]} deriving Show
data ArrivalList     = ArrivalList  {arrivalList  :: [Arrival]}  deriving Show

data ResultSet
     = ResultSet     { locations    :: LocationList
                      ,arrivals     :: ArrivalList
                      ,queryTime    :: Text
                     } deriving Show
  
data Location
     = Location      { loc_desc           :: Text
                      ,loc_locid          :: Int
                      ,loc_dir            :: Text
                      ,loc_lng            :: Double
                      ,loc_lat            :: Double
                     } deriving Show

data Arrival
     = Arrival       { arr_detour         :: Bool
                      ,arr_status         :: Text
                      ,arr_locid          :: Int
                      ,arr_block          :: Int
                      ,arr_scheduled      :: Text
                      ,arr_shortSign      :: Text
                      ,arr_dir            :: Int
                      ,estimated      :: Text
                      ,route          :: Int
                      ,departed       :: Bool
                      ,blockPosition  :: BlockPosition
                      ,fullSign       :: Text
                      ,piece          :: Text
                     } deriving Show
                          
data BlockPosition  
     = BlockPosition { bp_at                 :: Text
                      ,bp_feet               :: Int
                      ,bp_lng                :: Double
                      ,bp_trip               :: Trip
                      ,bp_lat                :: Double
                      ,bp_heading            :: Int 
                      } deriving Show

data Trip           
     = Trip          { trip_progress      :: Int
                      ,trip_desc          :: Text
                      ,trip_pattern       :: Int
                      ,trip_dir           :: Int
                      ,trip_route         :: Int
                      ,trip_tripNum       :: Int
                      ,trip_destDist      :: Int
                     } deriving Show

baseURL :: String
baseURL = "http://developer.trimet.org/ws/V1/arrivals/appID/3B5489BFA2CDF3D5711521B76/json/true/"

stopID  :: String
stopID   = "9843"

arrivalURL :: String -> String
arrivalURL id  = (baseURL ++ "locIDs/" ++ id ++ "/")

getJSON :: String -> IO B.ByteString
getJSON s = simpleHttp (arrivalURL s)

main :: IO()
main = do 
       json <- getJSON stopID
       putStrLn (show (decode json :: (Maybe Value)))
       putStrLn (show (decode json :: Maybe ResultSet))
       

instance FromJSON TripList where
  parseJSON (Object o) =
    TripList <$> (o .: "trip")
  parseJSON _ = mzero

instance FromJSON Trip where
  parseJSON (Object o) =
    Trip <$> (o .: "progress")
         <*> (o .: "desc")
         <*> (o .: "pattern")
         <*> (o .: "dir")
         <*> (o .: "route")
         <*> (o .: "tripNum")
         <*> (o .: "destDist")
  parseJSON _ = mzero

-- TODO: Make ArrivalList
-- This has to have all the fields in arrival
-- I don't know what it will look like.

instance FromJSON ArrivalList where
  parseJSON (Object o) =
    ArrivalList <$>  ((o .: "resultSet") >>= (.: "arrival"))
  parseJSON _ = mzero

instance FromJSON Arrival where
  parseJSON (Object o) =
    Arrival <$> (o .: "detour")
            <*> (o .: "status")
            <*> (o .: "locid")
            <*> (o .: "block")
            <*> (o .: "scheduled")
            <*> (o .: "shortSign")
            <*> (o .: "dir")
            <*> (o .: "estimated")
            <*> (o .: "route")
            <*> (o .: "departed")
            <*> (o .: "blockPosition")
            <*> (o .: "fullSign")
            <*> (o .: "piece")
  parseJSON _ = mzero

instance FromJSON LocationList where
  parseJSON (Object o) =
    LocationList <$> (o .: "location")
  parseJSON _ = mzero

instance FromJSON Location where
  parseJSON (Object o) =
    Location <$> (o .: "desc")
              <*> (o .: "locid")
              <*> (o .: "dir")
              <*> (o .: "lng")
              <*> (o .: "lat")
  parseJSON _ = mzero

instance FromJSON BlockPosition where
  parseJSON (Object o) =
    BlockPosition <$> (o .: "at")
              <*> (o .: "feet")
              <*> (o .: "lng")
              <*> (o .: "trip")
              <*> (o .: "lat")
              <*> (o .: "heading")
  parseJSON _ = mzero

instance FromJSON ResultSet where
  parseJSON (Object o) =
    ResultSet <$> ((o .: "resultSet") >>= (.: "location"))
              <*> ((o .: "resultSet") >>= (.: "arrival"))
              <*> ((o .: "resultSet") >>= (.: "queryTime"))
  parseJSON _ = mzero
