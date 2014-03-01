{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Main where

import Happstack.Server
import Happstack.Server.Monads
import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy.UTF8       as LU (toString, fromString)
import Data.ByteString.Lazy.Internal
import Control.Monad
import TrimetServiceFunctions
import Foreign.Marshal.Unsafe

instance ToMessage (IO Data.ByteString.Lazy.Internal.ByteString) where
   toContentType _ = B.pack "text/plain; charset=UTF-8"
   toMessage = unsafeLocalState

main :: IO ()
main = do simpleHTTP nullConf $ msum [ Happstack.Server.dir "arrivals" $ 
                                                                  path $
                                                              \s -> ok $ do json <- getArrivals s
                                                                            return json      
                                     ]
