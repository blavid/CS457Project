{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module HtmlStrings where

import Data.Text

arrivalsMainPage = (htmlHead.htmlBody) (htmlGlue [textBox "Stop ID" "arrivalsText", arrivalsButton, arrivalsJS])

htmlGlue :: [Text] -> Text
htmlGlue = Prelude.foldl1 append

htmlHead   :: Text -> Text
htmlHead s = htmlGlue [htmlMeta, "<html>", s, "</html>"]

htmlBody   :: Text -> Text
htmlBody s = htmlGlue ["<body>", s, "</body>"]

htmlTitle   :: Text -> Text
htmlTitle s = htmlGlue ["<title>", s, "</title>"]

htmlMeta   :: Text
htmlMeta = "<meta content='text/html;charset=utf-8' charset='UTF-8'>"

textBox           :: Text -> Text -> Text
textBox label id  = htmlGlue [label,": <input id='", id, "' type='text'><br>"] 

htmlButton          :: Text -> Text -> Text
htmlButton label id = htmlGlue ["<button id='", id, "' class='float-left submit-button' >", label, "</button>"]

arrivalsJS = "<script type='text/javascript'> document.getElementById('arrivalsButton').onclick = function () { location.href = 'http://192.241.236.98:8000/arrivals/' + document.getElementById('arrivalsText').value + '/';    }; </script>"

geolocationJS = "<script type='text/javascript'> var options = {  enableHighAccuracy: true,   timeout: 5000,   maximumAge: 0 }; function success(pos) { var crd = pos.coords; /*CODEHERE*/ }; function error(err) {  console.warn('ERROR(' + err.code + '): ' + err.message); }; function getLocation() { navigator.geolocation.getCurrentPosition(success, error, options); } </script>"
