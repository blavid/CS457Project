> {-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
> module HtmlBuilder where

The purpose of this module is to generate HTML responses.  Responses
will be either fairly static, or dynamic and based on a ResultSet that
is passed in, which is filled with data from a JSON response from
the Trimet web server.

The imports are related to this task.   
 
> import qualified Data.Text as D
> import HtmlStrings
> import TrimetDataTypes
> import Data.Time.Format 
> import Data.Time.Clock
> import Locale
> import Data.Time.LocalTime

 
The next few pages are static pages, which provide the user with an 
interface to interact with the application.  Because of using the Text
data type, each portion is put into a list, then the Text concatonate
is used to combine them into a single Text.  This same pattern occurs
throughout the rest of the module.

> homePage         :: D.Text
> homePage          = (htmlHead . htmlBody)
>                     (dconcat [
>                               header "Home Page"
>                              ,footer
>                              ])

> arrivalsMainPage :: D.Text 
> arrivalsMainPage = (htmlHead.htmlBody)
>                    (dconcat [
>                              header "Finds stops by Stop ID"
>                            , textBox "Stop ID" "arrivalsText"
>                            , htmlButton "Get Arrivals" "arrivalsButton"
>                            , footer
>                            , javascript url
>                            ])
>               where url = "http://192.241.236.98/javascript/arrivals.js"


The next functions are involved with generating a page listing of 
locations and their arrivals.  A lot of work is involved in this
and we'll cover each function in turn.

The first one takes the ResultSet data type.  It decorates the results
with some html, and includes the tableStyle, as well as calling a
function to take apart the ResuiltSet.
 
> arrivalPageListing    :: ResultSet -> D.Text
> arrivalPageListing rs = (htmlHead.htmlBody) 
>                         (dconcat 
>                             [
>                              header "Stops for requested locations"
>                             ,arrivalParseResultSet rs
>                             ,footer
>                             ,tableStyle
>                             ]
>                          )

> stopsNearbyListing rs = (htmlHead.htmlBody) 
>                         (dconcat [(stopsParseResultSet rs)
>                                  ,tableStyle])

> stopFinderForm    :: D.Text
> stopFinderForm      = 
>      dconcat  [
>                "<p>Enter your local coordinates or click 'Show my "
>               ,"Location' to automatically fill it in.</p>"
>               ,textBox "Long." "longitudeText"
>               ,textBox "Lat. " "latitudeText"
>               ,textBox "Radius" "radius"
>               ,radioButton "units" "feet" "feet" True, "<br>"
>               ,radioButton "units" "meters" "meters" False, "<br>"
>               ,htmlButton "Search" "nearbyStopsButton"
>               ]

> stopFinderMainPage :: D.Text
> stopFinderMainPage  = 
>      (htmlHead.htmlBody) 
>      (dconcat 
>          [
>           header "Find Stops Nearby"
>          ,stopFinderForm
>          ,showLocation
>          ,javascript geourl
>          ,javascript nearbyurl
>          ,footer
>          ])
>      where geourl = "http://192.241.236.98/javascript/geofindme.js"
>            nearbyurl =  "http://192.241.236.98/javascript/nearbystops.js"
 
This funtion begins to break the ResultSet into it's parts.  It calls
getLocations, which takes a list of Maybe Arrivals, and a list of 
Maybe Locations.

> arrivalParseResultSet    :: ResultSet -> D.Text
> arrivalParseResultSet rs = dconcat ["<p>"
>                                  , getLocations (arrivals rs) (locations rs)
>                                  , "</p>"]

> stopsParseResultSet    :: ResultSet -> D.Text
> stopsParseResultSet rs = dconcat ["<p>"
>                                  , getStops (locations rs)
>                                  , "</p>"]

Because we might or might not get any Locations or Arrivals, this
function pattern matches on different cases.  If we get arrivals,
but no location, we give a message saying we didn't get anything.
This is because arrivals are associated with a location, but if 
there is no location, there is no arrival!  If there are no arrivals,
but there are locations, then we print those locations.  If there
are both, then we have to print each location as a table header,
and then print each arrival as a table row, using a guard to 
make sure the arrivals stop id matches the locations.
 
> getLocations    :: Maybe [Arrival] -> Maybe [Location] -> D.Text
> getLocations x Nothing = "There is no stop associated with this Stop ID."
> getLocations Nothing (Just ls) = 
>             dconcat [arrivalTable ((dconcat.inner) l) | l <- ls]
>             where inner l   = [decLocs l, "No arrivals within the next hour"]
>                   decLocs l = (tableRow.tableHeader) (parseLocation l)
> getLocations (Just as) (Just ls) = 
>             dconcat [ arrivalTable (dconcat (inner l as)) | l <- ls]
>             where inner l as = [decLocs l, getArrivals l as]
>                   decLocs l  = (tableRow.tableHeader)(parseLocation l)

> getStops   :: Maybe [Location] -> D.Text
> getStops Nothing = "There are no stops nearby."
> getStops (Just stops) = 
>             stopsTable (dconcat [ (decLocs stop ) | stop <- stops])
>             where decLocs stop  = (tableRow.tableHeader)(parseLocation stop)


For each location, this will build the data stored into the table
header.  This data is information related to the stop, including
calling the function to generate a static google map to display 
the locatin on a map.
 
> parseLocation   :: Location -> D.Text
> parseLocation l = 
>             dconcat [ "Stop Info: ", htmlLink (aLink l) (stopId l), 
>                                 " ", (D.pack.loc_desc) l,
>                                 " ", googleMapLink [((loc_lat l),(loc_lng l), "Stop")]] 
>                  where aLink l = (dconcat [D.pack ("/arrivals/" ++ (show . loc_locid) l)])
>                        stopId l = ((D.pack . show . loc_locid) l)

getArrivals takes the list of arrivals and the current location stop id,
and builds the table rows for each arrival at that stop id.  The list of
arrivals can be arrivals from multiple stops, so a guard is used to 
only add in arrivals who are related to the current stop id.

> getArrivals      :: Location -> [Arrival] -> D.Text
> getArrivals l as = (D.pack.concat) 
>                    [ theString a | a <- as, loc_locid l == arr_locid a]
>                         where theString a = "<tr><td>" 
>                                           ++ (parseArrival a l) 
>                                           ++  "</td><tr>" 

For each arrival, the data for a table entry is created here.

> parseArrival     :: Arrival -> Location -> String
> parseArrival a l = concat ["Route: ", (show.route) a
>                        , " | Sign: ", arr_shortSign a
>                        , " | Scheduled: ", (timeFuncs.arr_scheduled) a
>                        , " | Estimated: ", (getEstimate.estimated) a
>                        , " | ", generateArrivalMap (blockPosition a) l]
>                         where timeFuncs = (convertTime.parseLocalTime)

> generateArrivalMap            :: Maybe BlockPosition -> Location -> String
> generateArrivalMap Nothing l  = "No map available."
> generateArrivalMap (Just b) l = D.unpack (googleMapLink [(loc_lat l, loc_lng l, "S"), (bp_lat b, bp_lng b, "V")])

The next three functions are for parsing the time strings returned from
the JSON objects.  The goal is to present the times in a more human
readable manner.  parseLocalTime takes the string and tries to parse
it based on a format specifier.  Some of the trailing characters are
stripped off because there was not a good format specifier in the library
we used.  If the time read does not match the specified format, it is read
in as Nothing.  After parsing, we then print it out in a new format.  
getEstimate is used for the time estimate, as it isn't always available.

> parseLocalTime   :: String -> Maybe LocalTime
> parseLocalTime a = parseTime defaultTimeLocale "%FT%T" (take 19 a)

> convertTime     :: Maybe LocalTime -> String
> convertTime Nothing = "No time."
> convertTime (Just x) = formatTime defaultTimeLocale "%r" x

> getEstimate :: Maybe String -> String
> getEstimate Nothing = "none"
> getEstimate (Just x) = (convertTime.parseLocalTime) x

This function creates the URL and link for a google map static image.

> googleMapLink :: [(Double,Double, D.Text)] -> D.Text
> googleMapLink coords = htmlLink (dconcat [googleMapsBaseLink, googleMapsCenter center, combined]) "Map"
>   where dToS   = (D.pack.show)
>         center = dconcat [(dToS.frst.head)coords, ",", (dToS.scnd.head)coords]
>         combined = (dconcat.concat) [ [googleMapsMarkers (thrd c), (dToS.frst) c, ",", (dToS.scnd) c, googleMapsSpacer] | c <- coords ] 


> frst :: (a, b, c) -> a
> frst (a, b, c) = a
> scnd :: (a, b, c) -> b
> scnd (a, b, c) = b
> thrd :: (a, b, c) -> c
> thrd (a, b, c) = c
