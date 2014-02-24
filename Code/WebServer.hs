module Main where

import Control.Monad    (msum)
import Happstack.Server (nullConf, simpleHTTP, ok, dir, path, seeOther)

main :: IO ()
main = simpleHTTP nullConf $
  msum [ dir "hello" $ path $ \s -> ok $ "Hello, " ++ s
       , seeOther "/hello/Haskell" "/hello/Haskell"
       ]
