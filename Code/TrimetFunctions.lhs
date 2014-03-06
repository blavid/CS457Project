> {-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
> module TrimetFunctions where
 
> import Data.Aeson
> import Data.ByteString.Lazy (ByteString)
> import Network.HTTP.Conduit (simpleHttp)
 
> baseURL :: String
> baseURL = "http://developer.trimet.org/ws/V1/arrivals/appID/3B5489BFA2CDF3D5711521B76/json/true/"
 
> stopID  :: String
> stopID   = "9843"
 
Arrival WebService:

> arrivalURL :: String -> String
> arrivalURL id  = baseURL ++ "locIDs/" ++ id ++ "/"
 
 
 
Example: callWebService (getArrivalURL "1234")

> callWebService :: String -> IO ByteString
> callWebService s = simpleHttp (s)
