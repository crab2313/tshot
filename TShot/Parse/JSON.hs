module TShot.Parse.JSON 
    (
     thumbsFromJSON,
     videosInfoFromJSON
    ) where

import TShot.Type

import Data.Maybe (fromJust)
import Data.Ratio (numerator, denominator)
import Network.URI (unEscapeString)
import Text.JSON (decode, fromJSObject, fromJSString, JSValue(..), Result(..))

thumbsFromJSON :: String -> [Thumbnail]
thumbsFromJSON = concat . map (listToThumbs . getSNPTList) . getArray
    where getArray = fromArray . getResList
          fromArray (JSArray a) = a
          fromArray _ = []
          getSNPTList = getObject "snpt_list"
          getResList = getObjectByJSON "res_list"
          
listToThumbs :: JSValue -> [Thumbnail]
listToThumbs (JSArray a) = map (thumbFromString . getSNPTUrl) a
    where thumbFromString (JSString s) = Thumbnail $ fromJSString s
          getSNPTUrl = getObject "snpt_url"

videosInfoFromJSON :: String -> [(VideoID, String)]
videosInfoFromJSON = map getIDAndName . getJSONSubList . getJSONResp
    where getJSONResp = getObjectByJSON "resp"

getJSONSubList :: JSValue -> [JSValue]
getJSONSubList (JSObject x) = fromArray $ lookSubList $ fromJSObject x
		where lookSubList sl = fromJust (lookup "subfile_list" sl)
		      fromArray (JSArray arr) = arr

getIDAndName :: JSValue -> (VideoID, String)
getIDAndName jv = (index jv, name jv)
	where name = jsVToName . lookName "name"
	      index = jsVToID . lookName "index"
	      lookName n = fromJust . lookup n . fromJSObject . fromValue 
	      fromValue (JSObject o) = o
	      jsVToName (JSString s) = unEscapeString $ fromJSString s
	      jsVToID :: JSValue -> VideoID
	      jsVToID (JSRational _ ra) = floor (n/d)
	      	where n = fromInteger $ numerator ra
		      d = fromInteger $ denominator ra

-- get JSON Object from a string
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