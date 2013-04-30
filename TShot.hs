module TShot where

import Network.HTTP
import Text.JSON
import TShot.Type
import TShot.Network

parseLink :: Link -> HashCode
parseLink = undefined

getJSON :: Link -> String
getJSON = undefined

getImage :: Link -> IO ()
getImage = undefined

getVideos :: Link -> IO [VideoID]
getVideos = undefined
