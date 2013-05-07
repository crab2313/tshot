module TShot.Parse.Link where

import Data.Char (toUpper)
import Codec.Binary.Base64.String (decode)

parseLink :: String -> String
parseLink link = case takeWhile (/= ':') link of
                   "magnet" -> parseMagnet link
		   "thunder" -> parseThunder link
                   _ -> if length link == 40 then map toUpper link
                                  else error $ "What's the link? " ++ link
                                                  
parseMagnet :: String -> String
parseMagnet =
    map toUpper . dropColon . dropColon . dropColon . takeWhile (/= '&')
        where dropColon = tail . dropWhile (/= ':')

parseThunder :: String -> String
parseThunder link = parseBase64 $ drop 8 [c | c <- link, c /= '/']
	where parseBase64 = parseLink . strip . decode
	      strip str = take (len-4) $ drop 2 str 
	      	where len = length str
          
-- TODO: parseThunder parseEd2k parseFlashGet
