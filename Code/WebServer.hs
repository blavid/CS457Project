{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Main where

import Happstack.Server
import Happstack.Server.Monads
import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy.UTF8       as LU (toString, fromString)
import qualified Data.Text.Lazy.Encoding         as LT
import Data.ByteString.Lazy.Internal
import Data.ByteString.Lazy
import Data.Text
import qualified Data.ByteString                 as BS
import Control.Monad
import Control.Applicative
import Data.Char
import TrimetFunctions
import TrimetDataTypes
import HtmlBuilder
import Data.Aeson
import Foreign.Marshal.Unsafe

instance ToMessage Text where
   toContentType _ = B.pack "text/html; charset=UTF-8"
   toMessage = LU.fromString.Data.Text.unpack

main :: IO ()
main = do simpleHTTP nullConf $ msum [  Happstack.Server.dir "arrivalsPage" $ ok arrivalsMainPage
                                      , Happstack.Server.dir "arrivals" $ 
                                                                   path $ 
                                                               \s -> ok $ arrivalsMatch s 
                                      , Happstack.Server.dir "stopFinderPage" $ ok "woooot"
                                     ]

arrivalsMatch stopId = Data.Text.pack $ unsafeLocalState
                     $ do json <- (eitherDecode <$> (getArrival stopId)) :: IO (Either String ResultSet)
                          case json of
                            Left err -> return ("Err: " ++ err)
                            Right rs -> return (show rs)




