module TShot.Parse.Link where



parseLink :: String -> String
parseLink = parseMagnet

parseMagnet :: String -> String
parseMagnet link = (dropColon . dropColon . dropColon . takeWhile (/= '&')) link
		where dropColon = tail . dropWhile (/= ':')
