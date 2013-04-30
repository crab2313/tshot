module TShot.Network 
	(
	getIDByHash,
	getImageByID,
	testCode
	)
	where

import TShot.Type

import Data.Ratio
import Data.Maybe
import Network.HTTP
import Text.JSON


tsHost :: Link
tsHost = "http://i.vod.xunlei.com/"

defaultUserAgent = undefined

idLink :: HashCode -> Link
idLink hash = tsHost ++ "/req_subBT/info_hash/" ++ hash ++ "/req_num/2000/req_offset/0/"

imageLink :: HashCode -> VideoID -> Link
imageLink hash i = tsHost ++ "req_screensnpt_url?userid=5&url=bt://" ++ hash ++ "/" ++ (show i)

testHashCode = "1A046C74B19DBA73E8CE0FDE584349F941AF6A55"

testCode = do x <- getIDByHash testHashCode
	      mapM getImage x
	      where getImage = getImageByID testHashCode

-- getImageByID:
-- getImageByID :: VideoID -> [Link]
getImageByID hash i = do
	rsp <- simpleHTTP (getRequest (imageLink hash i))
	body <- getResponseBody rsp
	(return . getResList) body

getResList :: String -> JSValue
getResList = getObjectByJSON "res_list"

-- getIDByHash: 
getIDByHash :: HashCode -> IO [VideoID]
getIDByHash hash = do
  rsp <- simpleHTTP (getRequest (idLink hash))
  body <- getResponseBody rsp
  (return . map getJSONIndex . getJSONSubList . getJSONResp) body

getJSONResp :: String -> JSValue
getJSONResp = getObjectByJSON "resp"

getJSONSubList :: JSValue -> [JSValue]
getJSONSubList (JSObject x) = (fromArray . lookSubList) (fromJSObject x)
		where lookSubList sl = fromJust (lookup "subfile_list" sl)
		      fromArray (JSArray arr) = arr

getJSONIndex = jsVToID . fromJust . lookup "index" . fromJSObject . fromValue 
	where fromValue (JSObject o) = o
	      jsVToID :: JSValue -> VideoID
	      jsVToID (JSRational _ ra) = floor (n/d)
	      	where n = fromInteger $ numerator ra
		      d = fromInteger $ denominator ra

-- get JSON object
getObjectByJSON :: String -> String -> JSValue
getObjectByJSON name json = (getObject name . fromResult) x
		where x = decode json :: Result JSValue
		      fromResult (Ok x) = x
		      fromResult (Error x) = undefined

getObject :: String -> JSValue -> JSValue
getObject name = fromJust . fromObject 
		where fromResult (Ok x) = x
		      fromResult (Error x) = undefined
		      fromObject (JSObject y) = lookup name (fromJSObject y)
