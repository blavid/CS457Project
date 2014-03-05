{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module HtmlBuilder where

import qualified Data.Text as D
import HtmlStrings
import TrimetDataTypes

arrivalsMainPage :: D.Text 
arrivalsMainPage = (htmlHead.htmlBody) (dconcat [textBox "Stop ID" "arrivalsText", htmlButton "Get Arrivals" "arrivalsButton", arrivalsJS])

arrivalPageListing    :: ResultSet -> D.Text
arrivalPageListing rs = (htmlHead.htmlBody) (dconcat [(arrivalParseResultSet rs), tableStyle])

arrivalParseResultSet    :: ResultSet -> D.Text
arrivalParseResultSet rs = dconcat ["<p>", getLocations (locations rs), "</p><p>", getArrivals (arrivals rs), "</p>"]

getLocations    :: [Location] -> D.Text
getLocations ls = dconcat [ arrivalTable ( (tableRow.tableHeader)(parseLocation l)) | l <- ls]

parseLocation   :: Location -> D.Text
parseLocation l = dconcat [ "Stop Info: ",  (D.pack.show.loc_locid) l, " ", (D.pack.loc_desc) l] 

getArrivals     :: [Arrival] -> D.Text
getArrivals as = (D.pack.concat) [ "<p>" ++ (show a) ++  "</p>" | a <- as]
