import Network.HTTP

baseArrivals = "http://developer.trimet.org/ws/V1/arrivals/appID/3B5489BFA2CDF3D5711521B76/json/true/"

arrivalFunction :: String -> IO String
arrivalFunction id = simpleHTTP (getRequest (baseArrivals ++ "locIDs/" ++ id ++ "/")) >>= getResponseBody
