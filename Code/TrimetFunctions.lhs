> {-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
> module TrimetFunctions where

This module is used to build URL's to use the Trimet API and use those
URL's to get a JSON response back from Trimet.  The parsing doesn't
happen here, instead, other portions of the application will instead
pass the JSON on.

Only two things are imported. simpleHttp uses a URL built from a String to
make a web service call.  It returns a ByteString, which is why we
also import that.  For printing the ByteString, putStrLn for
that ByteString is imported as well.
 
> import Data.ByteString.Lazy             (ByteString)
> import Data.ByteString.Lazy.Char8 as BS (putStrLn)
> import Network.HTTP.Conduit             (simpleHttp)
 
The baseURL and appID are both string constants.  The appID is a unique
identifier that must be applied for, which gives a developer access
to the Trimet API.

> baseURL :: String
> baseURL = "http://developer.trimet.org/ws"

> appID   :: String
> appID   = "/appID/3B5489BFA2CDF3D5711521B76/json/true"
 
Arrival WebService: 
This function builds a URL for calling the arrivals web service.  The URL
requires requires a comma delimited list of location ID's, and returns
back arrivals for the stops as JSON objects in the form of a String.

> arrivalURL     :: String -> String
> arrivalURL id  = baseURL ++ "/V1/arrivals" ++ appID ++ "/locIDs/" ++ id
 
Stop finder WebService:
This function builds the URL for calling the stop finder service.  The URL
requires a string that represents the latitude/longitude coordinates, in
the form of: '###.###########,###.##########'.  It also requires the 
units (feet or meter) and range.  It returns a JSON object represented
as a String.

> stopFinderURL                      :: String -> String -> String -> String
> stopFinderURL crds unit dist = baseURL ++ "/V1/stops" ++ appID 
>                             ++ "/ll/" ++ crds ++ "/" ++ unit ++ "/" ++ dist
 
Calling a web service:
This function takes a URL and uses the Conduit function simpleHttp to 
make a http call to the URL and return the response.  To be used in 
conjunction with the URL building functions.

> callWebService :: String -> IO ByteString
> callWebService s = simpleHttp (s)

Testing:
For testing, we just want to make sure that we are getting relevant 
JSON back.  For that, I have included a test function which will
call the callWebService function and print what it gets back.

> testURL   ::  String -> IO ()
> testURL url = do json <- callWebService url
>                  BS.putStrLn json

*TrimetFunctions> testURL (arrivalURL "4444")
{"resultSet":{"location":[{"desc":"SW Pilkington & Dawn","locid":4444,
"dir":"Southbound","lng":-122.733512926782,"lat":45.3881524549301}],
"arrival":[{"scheduled":"2014-03-10T08:20:46.000-0700","detour":true,
"shortSign":"36 To Tualatin P&R","locid":4444,"status":"scheduled",
"dir":0,"route":36,"departed":false,"block":3767,"piece":"1",
"fullSign":"36  South Shore to Tualatin Park & Ride"}],
"queryTime":"2014-03-08T13:29:52.338-0800"}}

*TrimetFunctions> testURL (stopFinderURL "45.5094804,-122.68135129999999" "feet" "50")
{"resultSet":{"location":[{"desc":"SW 4th & Hall","locid":12763,"dir":"Northbound","lng":-122.68129227783,"lat":45.5095756674907}],"queryTime":"2014-03-08T13:32:18.278-0800"}}

*TrimetFunctions> testURL (arrivalURL "-1")
{"resultSet":{"errorMessage":{"content":"Location id not found -1"},"queryTime":"2014-03-08T13:33:02.758-0800"}}

