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
import Data.Char
import TrimetServiceFunctions
import HtmlStrings
import Foreign.Marshal.Unsafe

instance ToMessage Text where
   toContentType _ = B.pack "text/html; charset=UTF-8"
   toMessage = LU.fromString.Data.Text.unpack

data MyString = MyString String

getStringFromMyString (MyString x) = x

main :: IO ()
main = do simpleHTTP nullConf $ msum [  Happstack.Server.dir "arrivalsPage" $ ok arrivalsMainPage
                                      , Happstack.Server.dir "arrivals" $ 
                                                                   path $ 
                                                               \s -> ok $ 
                                                       unsafeLocalState $ do json <- getArrivals s 
                                                                             return (Data.Text.pack.bsToStr.Prelude.head $ toChunks json)
                                     ]

arrivalPage        :: IO Text  -> IO Text
arrivalPage innard = do innard' <- innard
                        return $ htmlHead (htmlBody innard')
                       
bsToStr :: BS.ByteString -> String
bsToStr = Prelude.map (chr . fromEnum) . BS.unpack 
