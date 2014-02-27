{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Main where

import Happstack.Server
import Happstack.Server.Monads
import           Text.Blaze ((!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy.UTF8       as LU (toString, fromString)
import Data.ByteString.Lazy.Internal
import Control.Monad
import TrimetParseJSON
import Foreign.Marshal.Unsafe

appTemplate :: String -> [H.Html] -> H.Html -> H.Html
appTemplate title headers body =
    H.html $ do
      H.head $ do
        H.title (H.toHtml title)
        H.meta ! A.httpEquiv "Content-Type"
               ! A.content "text/html;charset=utf-8"
        sequence_ headers
      H.body $ do
        body

helloBlaze :: ByteString -> ServerPart Response
helloBlaze sss =
   ok $ toResponse $
    appTemplate "Hello, Blaze!"
                [H.meta ! A.name "keywords"
                        ! A.content "happstack, blaze, html"
                ]
                (H.p $ do "d" 
                          H.b "d")

instance ToMessage (IO Data.ByteString.Lazy.Internal.ByteString) where
   toContentType _ = B.pack "text/plain; charset=UTF-8"
   toMessage = unsafeLocalState

main :: IO ()
main = do simpleHTTP nullConf $ msum [ Happstack.Server.dir "arrivals" $ 
                                                                  path $
                                                      \s -> helloBlaze $ unsafeLocalState $ do json <- getJSON s
                                                                                               return json      
                                     ]
