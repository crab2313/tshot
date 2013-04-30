module Main(main) where

import Network.HTTP
import System.IO
import TShot
import TShot.Network
import System.Environment(getArgs)

main = do args <- getArgs
	  ids <- mapM getIDByHash args
	  re <- mapM putImage (zip args ids)
	  mapM putStrLn (concat $ concat $ concat re)
	  where putImage (hash, ids) = mapM (getImageByID hash) ids 
