module TShot.Parse.Link where

import Data.Char (toUpper)

parseLink :: String -> String
parseLink = map toUpper . parseMagnet

parseMagnet :: String -> String
parseMagnet link = (dropColon . dropColon . dropColon . takeWhile (/= '&')) link
		where dropColon = tail . dropWhile (/= ':')
