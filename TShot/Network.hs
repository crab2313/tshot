module TShot.Network where

import TShot.Type
import TShot.Parse.JSON

import System.IO (openBinaryFile, hPutStr, hClose, IOMode(..))
import Data.Ratio (numerator, denominator)
import Data.Maybe (fromJust)
import Network.URI (unEscapeString)
import Network.HTTP (getResponseBody, simpleHTTP, getRequest)
import Text.JSON (decode, fromJSObject, fromJSString, JSValue(..), Result(..))


tsHost :: Link
tsHost = "http://i.vod.xunlei.com/"

defaultUserAgent = undefined

idLink :: HashCode -> Link
idLink hash = tsHost ++ "/req_subBT/info_hash/" ++ hash ++ "/req_num/2000/req_offset/0/"

imageLink :: HashCode -> VideoID -> Link
imageLink hash i = tsHost ++ "req_screensnpt_url?userid=5&url=bt://" ++ hash ++ "/" ++ (show i)

downloadFile :: Link -> FilePath -> IO ()
downloadFile link fn = do 
	rsp <- simpleHTTP $ getRequest link
	body <- getResponseBody rsp
	fh <- openBinaryFile fn WriteMode
	hPutStr fh body
	hClose fh

fetchThumnail :: Thumbnail -> FilePath -> IO ()
fetchThumnail thumb = downloadFile (tbLink thumb)

fetchVideo :: FilePath ->(VideoID -> String -> Int -> String) -> Video -> IO ()
fetchVideo dir fname video = mapM_ fetch (zip [1..] thumbs)
	where fetch (i, t) = fetchThumnail t $ dir ++ "/" ++ fname id name i
	      thumbs = videoThumbs video
	      id = videoID video
	      name = videoName video

-- getThumbsByID:
getThumbsByID :: HashCode -> VideoID -> IO [Thumbnail]
getThumbsByID hash i = do
	rsp <- simpleHTTP $ getRequest $ imageLink hash i
	body <- getResponseBody rsp
	return $ thumbsFromJSON body

-- getVideosByHash: 
getVideosByHash :: HashCode -> IO [Video]
getVideosByHash hash = do
  rsp <- simpleHTTP $ getRequest $ idLink hash
  body <- getResponseBody rsp
  mapM pVideo $ videosInfoFromJSON body
  where pVideo (id, name) = do 
		thumbs <- getThumbsByID hash id
		return $ Video id name thumbs

-- get JSON object
getObjectByJSON :: String -> String -> JSValue
getObjectByJSON name json = getObject name $ fromResult x
		where x = decode json :: Result JSValue
		      fromResult (Ok x) = x
		      fromResult (Error x) = undefined

getObject :: String -> JSValue -> JSValue
getObject name = fromJust . fromObject 
		where fromResult (Ok x) = x
		      fromResult (Error x) = undefined
		      fromObject (JSObject y) = lookup name $ fromJSObject y
