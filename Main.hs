{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Data.Aeson
import GHC.Generics
import Control.Monad.IO.Class
import Network.HTTP.Simple
import Data.Text.Internal.Lazy

data MercuryRetro =
  MercuryRetro { is_retrograde :: Bool } deriving (Show, Generic)

instance FromJSON MercuryRetro
instance ToJSON MercuryRetro

getRetroJSON :: (MonadIO m) => m (Response MercuryRetro)
getRetroJSON = httpJSON "https://mercuryretrogradeapi.com"

responseHTMLContent :: (MonadIO m) => m (Response MercuryRetro) -> m Text
responseHTMLContent resp = isRetro <$> getResponseBody <$> resp

isRetro :: MercuryRetro -> Text
isRetro MercuryRetro {is_retrograde = ir} = 
  case ir of
    True -> "Take care, Mercury is in Retrograde!"
    False -> "Rejoice, Mercury is not in Retrograde!"

style :: Text
style = "<head><style>body {display: grid; place-items: center; color: white; \
        \ background-image: url(\"http://d1jqu7g1y74ds1.cloudfront.net/wp-content/uploads/2012/04/Venus-imaged-by-Magellan.jpg\"); \
        \ background-color: powderblue; position: relative; height: 100%; \
        \ margin: 0; font-size: 5em;} div {margin: 20px; text-align: center;}</style></head>"


main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    content <- responseHTMLContent getRetroJSON
    html $ mconcat [style, "<body><div>", content, "</div></body>"]
  get (regex ".*") $
    redirect "/"
