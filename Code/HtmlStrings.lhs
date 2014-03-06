> {-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
> module HtmlStrings where
 
> import Data.Text
 
> dconcat :: [Text] -> Text
> dconcat = Data.Text.concat
 
> htmlGlue :: [Text] -> Text
> htmlGlue = Prelude.foldl1 append
 
> htmlHead   :: Text -> Text
> htmlHead s = dconcat [htmlMeta, "<html>", s, "</html>"]
 
> htmlBody   :: Text -> Text
> htmlBody s = dconcat ["<body>", s, "</body>"]
 
> htmlTitle   :: Text -> Text
> htmlTitle s = dconcat ["<title>", s, "</title>"]
 
> htmlMeta   :: Text
> htmlMeta = "<meta content='text/html;charset=utf-8' charset='UTF-8'>"
 
> arrivalTable :: Text -> Text
> arrivalTable s = dconcat ["<table class='arrival_table'>", s, "</table>"]
 
> tableRow     :: Text -> Text
> tableRow s   = dconcat ["<tr>", s, "</tr>"]
 
> tableHeader  :: Text -> Text
> tableHeader s = dconcat ["<th>", s, "</th>"]
 
> tableData    :: Text -> Text
> tableData s  = dconcat ["<td>", s, "</td>"]
 
> textBox           :: Text -> Text -> Text
> textBox label id  = dconcat [label,": <input id='", id, "' type='text'><br>"] 
 
> htmlButton          :: Text -> Text -> Text
> htmlButton label id = dconcat ["<button id='", id, "' class='float-left submit-button' >", label, "</button>"]
 
> tableStyle :: Text
> tableStyle = pack "<style> table,th,td { border:1px solid black; border-collapse:collapse } </style>"
 
> arrivalsJS :: Text
> arrivalsJS = pack "<script type='text/javascript'> document.getElementById('arrivalsButton').onclick = function () { location.href = 'http://192.241.236.98:8000/arrivals/' + document.getElementById('arrivalsText').value + '/';    }; </script>"
 
> geolocationJS :: Text
> geolocationJS = pack "<script type='text/javascript'> var options = {  enableHighAccuracy: true,   timeout: 5000,   maximumAge: 0 }; function success(pos) { var crd = pos.coords; /*CODEHERE*/ }; function error(err) {  console.warn('ERROR(' + err.code + '): ' + err.message); }; function getLocation() { navigator.geolocation.getCurrentPosition(success, error, options); } </script>"
