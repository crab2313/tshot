module Network.TShot.Remote where

import Network.TShot.JSON
import Network.TShot.Types

import System.IO (openBinaryFile, hPutStr, hClose, IOMode(..))
import Network.HTTP
import Network.Browser
import Network.HTTP.Proxy

import Data.List (intercalate)

thunderHost :: Link
thunderHost = "http://i.vod.xunlei.com/"

userAgent =
    "Mozilla/5.0 (X11; Linux x86_64; rv:19.0) Gecko/20100101 Firefox/19.0"

urlToIds :: HashCode -> String
urlToIds hash = intercalate "/" uriList
  where uriList = [thunderHost, "req_subBT", "info_hash",
                   hash, "req_num", "2", "req_offset", "0"]

urlToImages :: HashCode -> VideoId -> String
urlToImages hash i = intercalate "/" uriList
  where uriList = [thunderHost, "req_screensnpt_url?userid=5&url=bt:/",
                   hash, show i]

getTShotRequest :: String -> Request_String
getTShotRequest = replaceHeader HdrUserAgent userAgent . getRequest 

-- old code bases
downloadFile :: Link -> FilePath -> IO ()
downloadFile link fn = do 
	rsp <- simpleHTTP $ getTShotRequest link
	body <- getResponseBody rsp
	fh <- openBinaryFile fn WriteMode
	hPutStr fh body
	hClose fh

fetchThumnail :: Thumbnail -> FilePath -> IO ()
fetchThumnail thumb = downloadFile (tbLink thumb)


fetchVideo :: FilePath ->(VideoId -> String -> Int -> String) -> Video -> IO ()
fetchVideo dir fname video = mapM_ fetch (zip [1..] thumbs)
	where fetch (i, t) = fetchThumnail t $ dir ++ "/" ++ fname id name i
	      thumbs = videoThumbs video
	      id = videoId video
	      name = videoName video

-- getThumbsByID:
getThumbsByID :: HashCode -> VideoId -> IO [Thumbnail]
getThumbsByID hash i = do
	rsp <- simpleHTTP $ getTShotRequest $ urlToImages hash i
	body <- getResponseBody rsp
	return $ thumbsFromJSON body

-- getVideosByHash: 
getVideosByHash :: HashCode -> IO [Video]
getVideosByHash hash = do
  rsp <- simpleHTTP $ getTShotRequest $ urlToIds hash
  body <- getResponseBody rsp
  mapM pVideo $ videosInfoFromJSON body
  where pVideo (id, name) = do 
		thumbs <- getThumbsByID hash id
		return $ Video id name thumbs


-- new code bases

-- need rename it soon
urlToTrt = urlToIds

acquireInfo :: String -> Proxy -> IO String
acquireInfo url proxy = do
    (_, rsp) <- browse $ do
        setProxy proxy
        request $ getTShotRequest url
    return $ rspBody rsp

acquireTrtInfo :: HashCode -> Proxy -> IO String
acquireTrtInfo hash = acquireInfo $ urlToTrt hash

acquireIndexInfo :: HashCode -> VideoId -> Proxy -> IO String
acquireIndexInfo hash i = acquireInfo $ urlToImages hash i
