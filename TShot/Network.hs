module TShot.Network where

import TShot.Type

import Data.Maybe
import Network.HTTP
import Text.JSON


tsHost :: Link
tsHost = "http://i.vod.xunlei.com/"

idLink :: HashCode -> Link
idLink hash = tsHost ++ "/req_subBT/info_hash/" ++ hash ++ "/req_num/2000/req_offset/0/"

testHashCode = "1A046C74B19DBA73E8CE0FDE584349F941AF6A55"

getVideoIDbyHash :: HashCode -> IO String
getVideoIDbyHash hash = do rsp <- simpleHTTP (getRequest (idLink hash))
			   getResponseBody rsp

testCode = do x <- getVideoIDbyHash testHashCode
	      return (getVideoIDbyJSON x)

getVideoIDbyJSON :: String -> JSValue
getVideoIDbyJSON json = fromJust (fromObject (fromResult x))
		where x = decode json :: Result JSValue
		      fromResult (Ok x) = x
		      fromResult (Error x) = undefined
		      fromObject (JSObject y) = lookup "resp" (fromJSObject y)


getVideoID (JSObject x) = (fromArray . lookSubList) (fromJSObject x)
		where lookSubList sl = fromJust (lookup "subfile_list" sl)
		      lookIndex jo = lookup "index" (fromJSObject jo)
		      fromArray (JSArray arr) = arr
