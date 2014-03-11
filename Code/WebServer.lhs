> {-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
> module Main where

This module is the main driver of the application.  It uses Happstack
to listen on a port for HTTP requests and respond accordingly.  Because
of this, it imports most of our other modules.  In addition, it imports
many other libraries.

There is a large headache involved with the different ByteStrings which
are used by the libraries.  Some of the imports are merely for conversion
to and from these different types of ByteStrings.  Other imports are
for running the parser, or required for Happstack.  Finally, we
also import an unsafeLocalState, the reasons for which will be discussed
below.
 
> import Happstack.Server                          as HS
> import qualified Data.ByteString.Char8           as B
> import qualified Data.ByteString.Lazy.UTF8       as LU (toString, fromString)
> import Data.Text                                 as DT (Text, unpack, pack)
> import Control.Monad (msum)
> import Control.Applicative
> import Data.Aeson (eitherDecode) 
> import Foreign.Marshal.Unsafe (unsafeLocalState)

Importing our own modules:

> import TrimetFunctions
> import TrimetDataTypes
> import HtmlBuilder
 
Everything needs to converge to a Text object in order to access
this custom instance of the ToMessage type class.  Happstack defines
String, but it sets the toContentType to "text/plain" instead of
"text/html".  Defining a type class for Text however, we can 
specify that we want the toContentType to be "text/html".

For the toMessage, it needs to be a ByteString, so we unpack
the Text into a String, then call the needed ByteStrings fromString
function.

> instance ToMessage Text where
>    toContentType _ = B.pack "text/html; charset=UTF-8"
>    toMessage = LU.fromString.DT.unpack
 
The main function.  The primary function is simpleHTTP, which takes a
configuration object, which contain some variables for the web server.
nullConf is the basic configuration:

nullConf :: Conf
nullConf = Conf
    { port      = 8000
    , validator = Nothing
    , logAccess = Just logMAccess
    , timeout   = 30
    }

For our purposes, we mainly paid attention to the port.  The other
items were not explored in any meaningful way.

simpleHTTP then finally takes a response in the form of a ServerPart IO a,
then finally it returns an IO ().  To generate the ServerPart IO a, msum
is used, along with a list of expressions that when evaluated, result in
a ServerPartT IO a.  The motiviation for the list, is it allows us to
return multiple responses, depending on the URL request.  The msum function
will return the first item in the list which pattern matches corectly. 
What it pattern matches on is the HS.dir "string".  If nothing matches, 
Happstack has a generic page it responds.

For any of the matches, we are calling a function from the HtmlBuilder
module, or calling an additional helper function to make a web service
call and parse the response before then generating a, HTML page.  One
of the gotchas we encountered is due to the list, and due to the fact
that sometimes we will have an IO Monad (when making a web service call)
and sometimes won't (when generating a static html page).  Because of 
that, we do unsafeLocalState after parsing in order to drop the IO 
monad.  Preferably we would have liked a proper solution, but in the 
interest of getting it done, we made the decision to introduce
impurities in our product.  

The "ok" are where the toMessage instance is invoked.  The function
takes it's argument and builds a ServerPartT by decorating it with
HTTP headers, including a HTTP 200 code for "OK".

> main :: IO ()
> main = do simpleHTTP nullConf $ msum [  HS.dir "arrivalsPage" $ ok arrivalsMainPage
>                                       , HS.dir "arrivals" $ 
>                                                      path $ 
>                                                  \s -> ok $ arrivalsMatch s 
>                                       , HS.dir "stopFinderPage" $ ok "woooot"
>                                      ]
 
This function is used when the user requests the arrivals for one
or more stop ID's.  First the web service URL is generated, the web 
service is then called giving back JSON.  That JSON is then attempted
to be parsed, which gives back Either an error message (String) or a
Haskell data type filled with the parsed values (ResultSet)  Then
we either print the error, or generate a page by passing HtmlBuilder
the result set.  Finally, we strip the IO from it and pass it back
to the web server.

> arrivalsMatch stopId = unsafeLocalState
>                      $ do result <- decoded :: IO (Either String ResultSet)
>                           case result of
>                             Left err -> return (DT.pack ("Err: " ++ err))
>                             Right rs -> return (arrivalPageListing rs)
>                        where decoded = eitherDecode <$> json
>                              json    = callWebService (arrivalURL stopId)
