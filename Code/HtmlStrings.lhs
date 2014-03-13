> {-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
> module HtmlStrings where
 
This module is simplistic and is used to store string constants
and build simple strings, which are related to the HTML responses
which are given to the user.  The things in here will be simple
HTML tags, JavaScript, and style information.  We did not use
a HTML library because of library conflicts that began to take
too much time away from project development time.

Some notes, we use the Data.Text import here because at the 
Web Server level of the application, a choice was made to use
Text due to being able to define it's instance.  The reason
for that can be found in the WebServer (Main) module.  As
a result, concatonation of Text objects is done using lists.

> import Data.Text

dconcat is just a relabeling of Data.Text.concat 

> dconcat :: [Text] -> Text
> dconcat = Data.Text.concat

The next few build up the base of a web page.  It was intended 
to come back later and refactor this into a data type, however
time did not permit.
 
> htmlHead   :: Text -> Text
> htmlHead s = dconcat ["<html>", s, "</html>"]
 
> htmlBody   :: Text -> Text
> htmlBody s = dconcat ["<body>", s, "</body>"]
 
> htmlTitle   :: Text -> Text
> htmlTitle s = dconcat ["<title>", s, "</title>"]
 
For the tables that are displayed, the base html is built in 
these next few functions.

> arrivalTable :: Text -> Text
> arrivalTable s = dconcat ["<table class='arrival_table'>", s, "</table>"]
 
> stopsTable :: Text -> Text
> stopsTable s = dconcat ["<table class='stops_table'>", s, "</table>"]
 
> tableRow     :: Text -> Text
> tableRow s   = dconcat ["<tr>", s, "</tr>"]
 
> tableHeader  :: Text -> Text
> tableHeader s = dconcat ["<th>", s, "</th>"]
 
> tableData    :: Text -> Text
> tableData s  = dconcat ["<td>", s, "</td>"]
 
Generic function to build a form.  Inside a form, there is a list of elements.

> form	           :: Text -> [Text] -> Text
> form name s = dconcat ["<form name='", name, "'>", dconcat s, "</form>"]

Generic function to build a textBox.

> textBox           :: Text -> Text -> Text
> textBox label id  = dconcat [label,": <input id='", id, "' type='text'><br>"] 
 
Generic function to build a button.

> htmlButton          :: Text -> Text -> Text
> htmlButton label id = dconcat ["<button id='", id, "' class='float-left submit-button' >", label, "</button>"]

Generic function to build a link.

> htmlLink :: Text -> Text -> Text
> htmlLink url label = dconcat ["<a href='", url, "'  target='_blank'>", label, "</a>"]

The next few functions are used to build a static google maps link.

> googleMapsBaseLink :: Text
> googleMapsBaseLink = "http://maps.googleapis.com/maps/api/staticmap?zoom=14&size=400x400&sensor=false&"

> googleMapsCenter :: Text -> Text
> googleMapsCenter center = dconcat ["center=", center]

> googleMapsSpacer :: Text
> googleMapsSpacer = "%7C"

> googleMapsMarkers :: Text 
> googleMapsMarkers = "&markers="

Defines the visual behavior of the tables used for displaying informationg.

> tableStyle :: Text
> tableStyle = "<style> table,th,td { border:1px solid black; border-collapse:collapse } </style>"
 
Our site doesn't use normal HTML form behavior.  Instead, this
Javascript redefines the behavior of the button to redirect to
a new URL, based on the information put into the arrivals box.

> arrivalsJS :: Text
> arrivalsJS =  "<script type='text/javascript'> document.getElementById('arrivalsButton').onclick = function () { location.href = 'http://192.241.236.98:8000/arrivals/' + document.getElementById('arrivalsText').value + '/';    }; </script>"
 
> nearbyStopsJS :: Text
> nearbyStopsJS  =  "<script type='text/javascript'> document.getElementById('nearbyStopsButton').onclick = function () { location.href = 'http://192.241.236.98:8000/stopFinder/' + document.getElementById('lattitudeText').value + ',' + document.getElementById('longitudeText').value + '/';    }; </script>"
 
This javascript gets the geolocation coordinates of the user.

> geolocationJS :: Text
> geolocationJS = "<script type='text/javascript'> var options = {  enableHighAccuracy: true,   timeout: 5000,   maximumAge: 0 }; function success(pos) { var crd = pos.coords; /*CODEHERE*/ }; function error(err) {  console.warn('ERROR(' + err.code + '): ' + err.message); }; function getLocation() { navigator.geolocation.getCurrentPosition(success, error, options); } </script>"
