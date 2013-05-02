module Main(main) where

import System.IO
import System.Environment(getArgs)

import TShot.Parse
import TShot.Network

main = do args <- getArgs
	  let hashs = map parseLink args
	  videos <- mapM getVideosByHash hashs
	  mapM_ (mapM_ $ fetchVideo dir fname) videos
	  where dir = "."
		fname id name n = name ++ show(id) ++ show(n)
