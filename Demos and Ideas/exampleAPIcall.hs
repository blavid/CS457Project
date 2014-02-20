import Network.HTTP

main = simpleHTTP (getRequest "http://developer.trimet.org/ws/V1/arrivals/locIDs/8886/appID/3B5489BFA2CDF3D5711521B76/json/true") >>= getResponseBody >>=  print
