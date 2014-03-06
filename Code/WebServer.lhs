> {-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
> module Main where
 
> import Happstack.Server
> import qualified Data.ByteString.Char8           as B
> import qualified Data.ByteString.Lazy.UTF8       as LU (toString, fromString)
> import Data.Text                                 as DT (Text, unpack, pack)
> import Control.Monad (msum)
> import Control.Applicative
> import TrimetFunctions
> import TrimetDataTypes
> import HtmlBuilder
> import Data.Aeson (eitherDecode) 
> import Foreign.Marshal.Unsafe (unsafeLocalState)
 
> instance ToMessage Text where
>    toContentType _ = B.pack "text/html; charset=UTF-8"
>    toMessage = LU.fromString.DT.unpack
 
> main :: IO ()
> main = do simpleHTTP nullConf $ msum [  Happstack.Server.dir "arrivalsPage" $ ok arrivalsMainPage
>                                       , Happstack.Server.dir "arrivals" $ 
>                                                                    path $ 
>                                                                \s -> ok $ arrivalsMatch s 
>                                       , Happstack.Server.dir "stopFinderPage" $ ok "woooot"
>                                      ]
 
> arrivalsMatch stopId = unsafeLocalState
>                      $ do json <- (eitherDecode <$> callWebService (arrivalURL stopId)) :: IO (Either String ResultSet)
>                           case json of
>                             Left err -> return (DT.pack ("Err: " ++ err))
>                             Right rs -> return (arrivalPageListing rs) 
