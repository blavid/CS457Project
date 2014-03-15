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
> htmlHead s = dconcat ["<!DOCTYPE html>\n<html>\n", s, "</html>\n"]
 
> htmlBody   :: Text -> Text
> htmlBody s = dconcat ["<body>\n", s, "</body>\n"]
 
> htmlTitle   :: Text -> Text
> htmlTitle s = dconcat ["<title>\n", s, "</title>\n"]
 
For the tables that are displayed, the base html is built in 
these next few functions.

> arrivalTable :: Text -> Text
> arrivalTable s = dconcat ["<table class='arrival_table'>\n", s, "</table>\n"]
 
> stopsTable :: Text -> Text
> stopsTable s = dconcat ["<table class='stops_table'>\n", s, "</table>\n"]
 
> tableRow     :: Text -> Text
> tableRow s   = dconcat ["<tr>\n", s, "</tr>\n"]
 
> tableHeader  :: Text -> Text
> tableHeader s = dconcat ["<th>\n", s, "</th>\n"]
 
> tableData    :: Text -> Text
> tableData s  = dconcat ["<td>\n", s, "</td>\n"]
 
Generic function to build a form.  Inside a form, there is a list of elements.

> form	           :: Text -> [Text] -> Text
> form name s = dconcat ["<form name='", name, "'>\n", dconcat s, "</form>\n"]

Generic function to build a textBox.

> textBox           :: Text -> Text -> Text
> textBox label id  = dconcat [label,": <input id='", id, "' type='text'><br>\n"] 
 
Generic function to build a radio button

> radioButton  :: Text -> Text -> Text -> Bool -> Text
> radioButton name value label checked = 
>    dconcat [
>            label
>           ,": <input type='radio' name='", name, "' value='", value, "' "
>           ,if checked then "checked>\n" else ">\n"
>            ]

Generic function to build a button.

> htmlButton          :: Text -> Text -> Text
> htmlButton label id = dconcat ["<button id='", id, "' class='float-left submit-button' >\n", label, "</button>\n"]

Generic function to build a link.

> htmlLink :: Text -> Text -> Text
> htmlLink url label = dconcat ["<a href='", url, "'  target='_blank'>\n", label, "</a>\n"]

The next few functions are used to build a static google maps link.

> googleMapsBaseLink :: Text
> googleMapsBaseLink = "http://maps.googleapis.com/maps/api/staticmap?zoom=14&size=400x400&sensor=false&"

> googleMapsCenter :: Text -> Text
> googleMapsCenter center = dconcat ["center=", center]

> googleMapsSpacer :: Text
> googleMapsSpacer = "%7C"

> googleMapsMarkers       :: Text -> Text 
> googleMapsMarkers label = dconcat [
>                            "&markers=label:"
>                            , label
>                            , googleMapsSpacer ]

Defines the visual behavior of the tables used for displaying informationg.

> tableStyle :: Text
> tableStyle = "<style>\n table,th,td { border:1px solid black; border-collapse:collapse }\n </style>\n"
 
Our site doesn't use normal HTML form behavior.  Instead, this
Javascript redefines the behavior of the button to redirect to
a new URL, based on the information put into the arrivals box.

> javascript :: Text -> Text
> javascript url = dconcat 
>                    [
>                     "<script src='"
>                    ,url
>                    ,"'></script>"
>                    ]

> onLoadJS       :: Text -> Text
> onLoadJS script = dconcat ["<script type='text/javascript'>\n  window.onload = ", script, ";\n</script>\n"]


> showLocation :: Text
> showLocation  = 
>  dconcat ["\n<p><button onclick=\"geoFindMe()\">Show my location</button></p>\n<div id=\"out\"></div>\n"]

> footer :: Text
> footer  = 
>    dconcat $ Prelude.map (append "\n") [
>          "<p>This is the footer</p>"
>         ,"<p>Other HTML elements can go in here.</p>"
>          ]

> header        :: Text -> Text
> header caption = 
>    dconcat $ Prelude.map (append "\n") [
>          "<a href='/'><img src='http://192.241.236.98/images/header.jpg'></a>"
>         ,"<h1>",caption,"</h1>"
>         ,"<p>This is the header</p>"
>         ,"<p>Other HTML elements can go in here.</p>"
>          ]
