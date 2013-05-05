module TShot.Parse.Link where

import Data.Char (toUpper)

parseLink :: String -> String
parseLink link = if length link == 40 then map toUpper link else 
		map toUpper $ parseMagnet link

parseMagnet :: String -> String
parseMagnet link = (dropColon . dropColon . dropColon . takeWhile (/= '&')) link
		where dropColon = tail . dropWhile (/= ':')
