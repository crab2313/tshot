module Main(main) where

import Data.List(intercalate)
import System.Environment(getArgs)

import Network.TShot.Parser
import Network.TShot.Remote

main = do
  args <- getArgs
  let hashs = map parseLink args
  videos <- mapM getVideosByHash hashs
  mapM_ (mapM_ $ fetchVideo dir fname) videos
      where dir = "."
	    fname id name n = intercalate "-" [eName, show(id), show(n), ".tb"]
                where eName = map (\c -> if c == '/' then '-' else c) name
                
