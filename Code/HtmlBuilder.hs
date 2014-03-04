{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module HtmlBuilder where

import qualified Data.Text as D
import HtmlStrings
import TrimetDataTypes

arrivalsMainPage :: D.Text 
arrivalsMainPage = (htmlHead.htmlBody) (D.concat [textBox "Stop ID" "arrivalsText", htmlButton "Get Arrivals" "arrivalsButton", arrivalsJS])

arrivalPageListing    :: ResultSet -> D.Text
arrivalPageListing rs = (htmlHead.htmlBody) (arrivalParseResultSet rs)

arrivalParseResultSet    :: ResultSet -> D.Text
arrivalParseResultSet rs = D.concat ["<p>", getLocations (locations rs), "</p><p>", getArrivals (arrivals rs), "</p>"]

getLocations    :: [Location] -> D.Text
getLocations ls = (D.pack.concat) [ "<p>" ++ (show l) ++ "</p>" | l <- ls]

getArrivals     :: [Arrival] -> D.Text
getArrivals as = (D.pack.concat) [ "<p>" ++ (show a) ++  "</p>" | a <- as]
