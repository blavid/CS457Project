import Data.Aeson
import Data.Attoparsec
import Data.ByteString
import Data.Maybe
import Network.HTTP
import Network.URI

main = do
       src <- openURL "http://www.reddit.com/user/chrissalij/about.json"
       print $ parse json src

openURL :: String -> IO ByteString
openURL url = getResponseBody =<< simpleHTTP (mkRequest GET (fromJust $ parseURI url))

