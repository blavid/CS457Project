{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module TrimetDataTypes where

import Data.Aeson
import Control.Applicative
import Data.Text
import Control.Monad

{-
Trimet returns a JSON response that contains a hierarchical dataset:
  resultSet
    LocationList [
      desc
      locid
      dir
      lng
      lat ]
    ArrivalList [
      detour
      status
      locid
      block
      etc...
      blockPosition [
	at
	feet
	lng
	trip [
	  progress
	  desc
	  etc... ]
	etc... ]
    queryTime

Full API documentation found at http://developer.trimet.org/ws_docs/

-}

-- Data Type Definitions and FromJSON Instance Definitions ---------------------

data ResultSet
     = ResultSet     { 
                       locations    :: [Location]
                      ,arrivals     :: [Arrival]
                      ,queryTime    :: String
                     } deriving Show

instance FromJSON ResultSet where
  parseJSON (Object o) = ResultSet <$> 
                         ((o .: "resultSet") >>= (.: "location"))
                     <*> ((o .: "resultSet") >>= (.: "arrival"))
                     <*> ((o .: "resultSet") >>= (.: "queryTime"))
  parseJSON _ = mzero

data Location
     = Location      { loc_desc           :: String
                      ,loc_locid          :: Int
                      ,loc_dir            :: String
                      ,loc_lng            :: Double
                      ,loc_lat            :: Double
                     } deriving Show

instance FromJSON Location where
  parseJSON (Object o) =
    Location <$> (o .: "desc")
              <*> (o .: "locid")
              <*> (o .: "dir")
              <*> (o .: "lng")
              <*> (o .: "lat")
  parseJSON _ = mzero

data Arrival
     = Arrival       { arr_detour         :: Bool
                      ,arr_status         :: String
                      ,arr_locid          :: Int
                      ,arr_block          :: Int
                      ,arr_scheduled      :: String
                      ,arr_shortSign      :: String
                      ,arr_dir            :: Int
                      ,estimated          :: Maybe String
                      ,route              :: Int
                      ,departed           :: Bool
                      ,blockPosition      :: Maybe BlockPosition
                      ,fullSign           :: String
                      ,piece              :: String
                     } deriving Show

instance FromJSON Arrival where
  parseJSON (Object o) =
    Arrival <$> (o .: "detour")
            <*> (o .: "status")
            <*> (o .: "locid")
            <*> (o .: "block")
            <*> (o .: "scheduled")
            <*> (o .: "shortSign")
            <*> (o .: "dir")
            <*> (o .:? "estimated")
            <*> (o .: "route")
            <*> (o .: "departed")
            <*> (o .:? "blockPosition")
            <*> (o .: "fullSign")
            <*> (o .: "piece")
  parseJSON _ = mzero

data BlockPosition  
     = BlockPosition { bp_at                 :: String
                      ,bp_feet               :: Int
                      ,bp_lng                :: Double
                      ,bp_trip               :: [Trip]
                      ,bp_lat                :: Double
                      ,bp_heading            :: Int 
                      } deriving Show

instance FromJSON BlockPosition where
  parseJSON (Object o) =
    BlockPosition <$> (o .: "at")
              <*> (o .: "feet")
              <*> (o .: "lng")
              <*> (o .: "trip")
              <*> (o .: "lat")
              <*> (o .: "heading")
  parseJSON _ = mzero

data Trip           
     = Trip          { trip_progress         :: Int
                      ,trip_desc             :: String
                      ,trip_pattern          :: Int
                      ,trip_dir              :: Int
                      ,trip_route            :: Int
                      ,trip_tripNum          :: String
                      ,trip_destDist         :: Int
                     } deriving Show

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
