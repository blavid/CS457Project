{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module TrimetFunctions where

import Data.Aeson
import Control.Applicative
import Data.Text
--import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
--import GHC.Generics
import TrimetDataTypes as T
import Foreign.Marshal.Unsafe

baseURL :: String
baseURL = "http://developer.trimet.org/ws/V1/arrivals/appID/3B5489BFA2CDF3D5711521B76/json/true/"

stopID  :: String
stopID   = "9843"

arrivalURL :: String -> String
arrivalURL id  = (baseURL ++ "locIDs/" ++ id ++ "/")

getArrival :: String -> IO B.ByteString
getArrival s = simpleHttp (arrivalURL s)

{-
doSomething :: IO (Either String ResultSet)
doSomething  = do
                 d <- (eitherDecode (getJSON stopID))
	         case d of
		   Left err -> ("Returned Nothing." ++ err)
		   Right rs -> rs
-}

-- Functions ---------------------------------------------------------
{-
getQueryTime :: ResultSet -> String
getQueryTime 
  = do
    x <- (eitherDecode (getJSON stopID)) :: Either String ResultSet
      case x of
        Left err -> "Error: " ++ err
        Right rs -> queryTime rs
-}

decodeResultSet :: B.ByteString -> Maybe ResultSet
decodeResultSet response = decode response

{-main :: IO()
main = do 
d <- (eitherDecode <$> (getJSON stopID)) :: IO (Either String ResultSet)
       case d of
         Left err -> putStrLn ("Error: " ++ err)
	 Right rs -> do
                       print (show (locations rs))
		       print (queryTime rs)
		       print (show (arrivals rs))
-}
