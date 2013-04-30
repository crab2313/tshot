module TShot.Parse where



parseLink :: String -> String
parseLink = parseMagnet

parseMagnet :: String -> String
parseMagnet link = (takeWhile (/= '&') . dropColon . dropColon . dropColon) link
		where dropColon = dropWhile (/= ':')
