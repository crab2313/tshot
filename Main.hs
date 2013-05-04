module Main(main) where

import Data.List(intercalate)
import System.Environment(getArgs)

import TShot.Parse.Link
import TShot.Network

main = do args <- getArgs
	  let hashs = map parseLink args
	  videos <- mapM getVideosByHash hashs
	  mapM_ (mapM_ $ fetchVideo dir fname) videos
	  where dir = "."
		fname id name n = intercalate "-" [name, show(id), show(n)]
