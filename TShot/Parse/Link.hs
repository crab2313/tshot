module TShot.Parse.Link where

import Data.Char (toUpper)

parseLink :: String -> String
parseLink link = case takeWhile (== ':') link of
                   "magnet" -> parseMagnet link
                   _ -> if length link == 40 then map toUpper link
                                              else error "What's the link?"
                                                  
parseMagnet :: String -> String
parseMagnet =
    map toUpper . dropColon . dropColon . dropColon . takeWhile (/= '&')
        where dropColon = tail . dropWhile (/= ':')

-- TODO: parseThunder parseEd2k parseFlashGet