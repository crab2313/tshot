module Main (main) where

import Data.List (intercalate)
import System.Environment (getArgs, getEnv)
import Control.Exception 
import System.IO.Error
import Control.Monad (guard)

import Network.HTTP.Proxy
import Network.TShot.Parser
import Network.TShot.Remote

main = do
  args <- getArgs
  let hashs = map parseLink args
  proxy <- checkProxy
  videos <- mapM (getVideosByHash proxy)hashs
  mapM_ (mapM_ $ fetchVideo dir fname) videos
      where dir = "."
	    fname id name n = intercalate "-" [eName, show(id), show(n), ".tb"]
                where eName = map (\c -> if c == '/' then '-' else c) name
                
-- checking whether we need a proxy connection
checkProxy :: IO Proxy
checkProxy = do
    envProxy <- tryJust (guard . isDoesNotExistError) $ getEnv "http_proxy"
    case envProxy of
        Left _ -> return noProxy
        Right proxy -> case parseProxy proxy of
            Just p -> return p
            Nothing -> return noProxy
