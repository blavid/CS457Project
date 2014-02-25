{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics

main = do
       src <- openURL "http://www.reddit.com/user/chrissalij/about.json"
       print $ parse json src

openURL :: String -> IO ByteString
openURL url = getResponseBody =<< simpleHttp (mkRequest GET (fromJust $ parseURI url))

