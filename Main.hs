module Main(main) where

import Network.HTTP
import System.IO
import TShot
import TShot.Network
import System.Environment(getArgs)

main = do args <- getArgs
	  ids <- mapM getIDByHash args
	  mapM_ (putStrLn . show) ids
