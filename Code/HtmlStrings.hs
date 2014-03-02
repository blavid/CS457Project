{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module HtmlStrings where

import Data.Text

arrivalsMainPage = (htmlHead.htmlBody) (Prelude.foldl1 append [arrivalsTextBox, arrivalsButton, arrivalsJS])

htmlHead   :: Text -> Text
htmlHead s = Prelude.foldl1 append [htmlMeta, "<html>", s, "</html>"]

htmlBody   :: Text -> Text
htmlBody s = Prelude.foldl1 append ["<body>", s, "</body>"]

htmlMeta   :: Text
htmlMeta = "<meta content='text/html;charset=utf-8' charset='UTF-8'>"

arrivalsTextBox = "Stop ID: <input id='arrivalsText' type='text' name='firstname'><br>" 

arrivalsButton = "<button id='arrivalsButton' class='float-left submit-button' >Get Arrivals</button>"

arrivalsJS = "<script type='text/javascript'> document.getElementById('arrivalsButton').onclick = function () { location.href = 'http://192.241.236.98:8000/arrivals/' + document.getElementById('arrivalsText').value + '/';    }; </script>"
