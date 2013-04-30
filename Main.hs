module Main(main) where

import Network.HTTP
import System.IO
import TShot
import TShot.Parse
import TShot.Network
import System.Environment(getArgs)

main = do args <- getArgs
	  ids <- mapM (getIDByHash . parseLink) args
	  re <- mapM putImage (zip args ids)
	  let cre = concat $ concat $ concat re
	  mapM download (zip [1..] cre)
	  where putImage (hash, ids) = mapM (getImageByID hash) ids 
		download (n, link) = downloadFile link (show n)
