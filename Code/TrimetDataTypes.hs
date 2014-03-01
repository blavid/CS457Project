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


--newtype LocationList = LocationList [Location]
--newtype ArrivalList  = ArrivalList [Arrival]
--newtype TripList     = TripList [Trip]
data TripList        = TripList     {triplist     :: [Trip]}     deriving Show
data LocationList    = LocationList {locationList :: [Location]} deriving Show
data ArrivalList     = ArrivalList  {arrivalList  :: [Arrival]}  deriving Show

data ResultSet
     = ResultSet     { 
-- locationList :: LocationList
-- arrivalList  :: ArrivalList
                      queryTime    :: Text
                     } deriving Show
  
data Location
     = Location      { desc           :: Text
                      ,locid          :: Int
                      ,dir            :: Text
                      ,lng            :: Double
                      ,lat            :: Double
                     } deriving Show

data Arrival
     = Arrival       { detour         :: Bool
                      ,status         :: Text
--                      ,locid          :: Int
                      ,block          :: Int
                      ,scheduled      :: Text
                      ,shortSign      :: Text
--                      ,dir            :: Int
                      ,estimated      :: Text
                      ,route          :: Int
                      ,departed       :: Bool
                      ,blockPosition  :: BlockPosition
                      ,fullSign       :: Text
                      ,piece          :: Text
                     } deriving Show
                          
data BlockPosition  
     = BlockPosition { at                 :: Text
                      ,feet               :: Int
--                      ,lng                :: Double
                      ,trip               :: Trip
--                      ,lat                :: Double
                      ,heading            :: Int 
                      } deriving Show

data Trip           
     = Trip          { progress      :: Int
--                      ,desc          :: Text
                      ,pattern       :: Int
--                      ,dir           :: Int
--                      ,route         :: Int
                      ,tripNum       :: Int
                      ,destDist      :: Int
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
       putStrLn (show (decode json :: (Maybe ResultSet)))

instance FromJSON TripList where
  parseJSON (Object o) =
    TripList <$> (o .: "trip")
  parseJSON _ = mzero

instance FromJSON Trip where
  parseJSON (Object o) =
    Trip <$> (o .: "progress")
         <*> (o .: "pattern")
         <*> (o .: "tripNum")
         <*> (o .: "destDist")
  parseJSON _ = mzero

instance FromJSON ResultSet where
  parseJSON (Object o) =
    ResultSet <$> ((o .: "resultSet") >>= (.: "queryTime"))
  parseJSON _ = mzero
