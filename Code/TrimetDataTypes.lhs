> {-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
> module TrimetDataTypes where 
> import Data.Aeson
> import Control.Applicative
> import Data.Text
> import Control.Monad

This module only defines data types and instances.  There aren't
any functions written, as the data types are written in the way
that causes their element accessor functions to be automatically
created.

The data types are designed based around the structure of the 
JSON that is returned from the Trimet web services.

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
 
 -- Data Type Definitions and FromJSON Instance Definitions ---------------------

The top level item returned is a result set, and it contains up to 3
different values.  The locations and arrivals values are not always
returned, but query time always is.  Thus, we define the datatype
to hold values for a list of locations (maybe!), a dlist of arrivals,
and the time it took to process the query.  

> data ResultSet
>      = ResultSet     { 
>                        locations    :: Maybe [Location]
>                       ,arrivals     :: Maybe [Arrival]
>                       ,queryTime    :: String
>                      } deriving Show
 
To feed the JSON into our data type, we must define an instance of
FromJSON, as part of the Aeson module.  It must contain the same
structure (order) of the data type.  It works by matching strings
to the key of the JSON.  So for locations, we check the value of 
resultSet, and try to see if there is a location key.  If not, we
put a Nothing in locations, otherwise we put the value into it.

When it tries to put the value of it into a type of Maybe [Location],
it will then need to parse he that value per the definition of Location,
which is included below. That same pattern is continued for the rest
of the data types.

> instance FromJSON ResultSet where
>   parseJSON (Object o) = ResultSet <$> 
>                          ((o .: "resultSet") >>= (.:? "location"))
>                      <*> ((o .: "resultSet") >>= (.:? "arrival"))
>                      <*> ((o .: "resultSet") >>= (.: "queryTime"))
>   parseJSON _ = mzero
 
> data Location
>      = Location      { loc_desc           :: String
>                       ,loc_locid          :: Int
>                       ,loc_dir            :: String
>                       ,loc_lng            :: Double
>                       ,loc_lat            :: Double
>                      } deriving Show
 
> instance FromJSON Location where
>   parseJSON (Object o) =
>     Location <$> (o .: "desc")
>               <*> (o .: "locid")
>               <*> (o .: "dir")
>               <*> (o .: "lng")
>               <*> (o .: "lat")
>   parseJSON _ = mzero
 
> data Arrival
>      = Arrival       { arr_detour         :: Bool
>                       ,arr_status         :: String
>                       ,arr_locid          :: Int
>                       ,arr_block          :: Int
>                       ,arr_scheduled      :: String
>                       ,arr_shortSign      :: String
>                       ,arr_dir            :: Int
>                       ,estimated          :: Maybe String
>                       ,route              :: Int
>                       ,departed           :: Bool
>                       ,blockPosition      :: Maybe BlockPosition
>                       ,fullSign           :: String
>                       ,piece              :: String
>                      } deriving Show
 
> instance FromJSON Arrival where
>   parseJSON (Object o) =
>     Arrival <$> (o .: "detour")
>             <*> (o .: "status")
>             <*> (o .: "locid")
>             <*> (o .: "block")
>             <*> (o .: "scheduled")
>             <*> (o .: "shortSign")
>             <*> (o .: "dir")
>             <*> (o .:? "estimated")
>             <*> (o .: "route")
>             <*> (o .: "departed")
>             <*> (o .:? "blockPosition")
>             <*> (o .: "fullSign")
>             <*> (o .: "piece")
>   parseJSON _ = mzero
 
> data BlockPosition  
>      = BlockPosition { bp_at                 :: String
>                       ,bp_feet               :: Int
>                       ,bp_lng                :: Double
>                       ,bp_trip               :: [Trip]
>                       ,bp_lat                :: Double
>                       ,bp_heading            :: Int 
>                       } deriving Show
 
> instance FromJSON BlockPosition where
>   parseJSON (Object o) =
>     BlockPosition <$> (o .: "at")
>               <*> (o .: "feet")
>               <*> (o .: "lng")
>               <*> (o .: "trip")
>               <*> (o .: "lat")
>               <*> (o .: "heading")
>   parseJSON _ = mzero
 
> data Trip           
>      = Trip          { trip_progress         :: Int
>                       ,trip_desc             :: String
>                       ,trip_pattern          :: Int
>                       ,trip_dir              :: Int
>                       ,trip_route            :: Int
>                       ,trip_tripNum          :: String
>                       ,trip_destDist         :: Int
>                      } deriving Show
 
> instance FromJSON Trip where
>   parseJSON (Object o) =
>     Trip <$> (o .: "progress")
>          <*> (o .: "desc")
>          <*> (o .: "pattern")
>          <*> (o .: "dir")
>          <*> (o .: "route")
>          <*> (o .: "tripNum")
>          <*> (o .: "destDist")
>   parseJSON _ = mzero
