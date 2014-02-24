import Network.HTTP
import Network.Stream

baseArrivals = "http://developer.trimet.org/ws/V1/arrivals/appID/3B5489BFA2CDF3D5711521B76/json/true/"

-- getArrivals    :: String -> IO (Result (Response String))
getArrivals id = simpleHTTP (getRequest (baseArrivals ++ "locIDs/" ++ id ++ "/")) >>= getResponseBody
