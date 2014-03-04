{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module HtmlBuilder where

import Data.Text
import HtmlStrings

arrivalsMainPage = (htmlHead.htmlBody) (htmlGlue [textBox "Stop ID" "arrivalsText", arrivalsButton "Get Arrivals" "arrivalsButton", arrivalsJS])
