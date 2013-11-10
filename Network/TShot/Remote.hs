{-# LANGUAGE OverloadedStrings #-}
module Network.TShot.Remote where

import Network.TShot.Types

import System.IO (openBinaryFile, hPutStr, hClose, IOMode(..))
import Network.HTTP
import Network.Browser
import Network.HTTP.Proxy
import Data.Aeson

import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate)
import qualified Data.ByteString.Lazy.Char8 as BL

thunderHost :: Link
thunderHost = "http://i.vod.xunlei.com/"

userAgent =
    "Mozilla/5.0 (X11; Linux x86_64; rv:19.0) Gecko/20100101 Firefox/19.0"

-- use something like URI builder
urlToTrt :: HashCode -> String
urlToTrt hash = intercalate "/" uriList
  where uriList = [thunderHost, "req_subBT", "info_hash",
                   hash, "req_num", "2", "req_offset", "0"]

urlToImages :: HashCode -> VideoId -> String
urlToImages hash i = intercalate "/" uriList
  where uriList = [thunderHost, "req_screensnpt_url?userid=5&url=bt:/",
                   hash, show i]

getTShotRequest :: String -> Request_String
getTShotRequest = replaceHeader HdrUserAgent userAgent . getRequest 

-- acquire information from server
acquireInfo :: String -> Proxy -> IO String
acquireInfo url proxy = do
    (_, rsp) <- browse $ do
        setProxy proxy
        request $ getTShotRequest url
    return $ rspBody rsp

acquireTrtInfo :: HashCode -> Proxy -> IO String
acquireTrtInfo hash = acquireInfo $ urlToTrt hash

acquireVideoThumbs :: HashCode -> VideoId -> Proxy -> IO String
acquireVideoThumbs hash i = acquireInfo $ urlToImages hash i

-- generate higher order structures from raw input
-- json related stuff
acquireThumbs :: HashCode -> VideoId -> Proxy -> IO (Maybe [String])
acquireThumbs hash i proxy = do
    info <- acquireVideoThumbs hash i proxy 
    case decode (BL.pack info) :: Maybe TrtVideoInfo of
        Just thumbsInfo -> if tVideoRet thumbsInfo /= 0
                           then return Nothing
                           else return $ Just $ stripThumbs thumbsInfo
        Nothing -> return Nothing
  where stripThumbs ti = map tVideoSNPTUrls $ tVideoRspSNPTList $
                         last $ tVideoResList ti

acquireTorrent :: HashCode -> Proxy -> IO (Maybe Torrent)
acquireTorrent hash proxy = do
    info <- acquireTrtInfo hash proxy
    case decode (BL.pack info) :: Maybe TrtInfo of
        Nothing -> return Nothing
        Just ti -> if (tInfoRspRet $ tInfoRsp ti) /= 0
                   then return Nothing
                   else stripTrtRsp $ tInfoRsp ti
                     
  where stripTrtRsp :: TrtInfoRsp -> IO (Maybe Torrent)
        stripTrtRsp rsp = do
            tVideo <- mapM stripSubfile $ tInfoRspSubfiles rsp
            return $ Just $ Torrent hash tVideo
        stripSubfile :: TrtInfoSubfile -> IO (Maybe Video)
        stripSubfile sub = do
            thumbs <- acquireThumbs hash index proxy
            case thumbs of
                Nothing -> return Nothing
                Just t -> return $ Just $ Video index name t size
          where index = tInfoSubfileIndex sub
                name = tInfoSubfileName sub
                size = tInfoSubfileSize sub

-- json related
-- TrtInfo
data TrtInfo = TrtInfo {
    tInfoRsp :: TrtInfoRsp
    } deriving (Show)

data TrtInfoRsp = TrtInfoRsp {
    tInfoRspUserId :: Int,
    tInfoRspRet :: Int,
    tInfoRspSubfiles :: [TrtInfoSubfile]
    -- info hash ?
    } deriving (Show)               

data TrtInfoSubfile = TrtInfoSubfile {
    tInfoSubfileName :: String,
    tInfoSubfileSize :: Int,
    tInfoSubfileIndex :: Int
    } deriving (Show)

instance FromJSON TrtInfo where
    parseJSON (Object v) = TrtInfo <$> v .: "resp"

instance FromJSON TrtInfoRsp where
    parseJSON (Object v) = TrtInfoRsp <$> v .: "userid" <*>
                           v .: "ret" <*> v .: "subfile_list"

instance FromJSON TrtInfoSubfile where
    parseJSON (Object v) = TrtInfoSubfile <$> v .: "name" <*>
                           v .: "file_size" <*> v .: "index"

-- TrtVideoInfo
data TrtVideoInfo = TrtVideoInfo {
    tVideoResList :: [TrtVideoRsp],
    tVideoRet :: Int
    } deriving (Show)

data TrtVideoRsp = TrtVideoRsp {
    tVideoRspGCID :: String,
    tVideoRspSNPTList :: [TrtVideoSNPT], 
    tVideoRspSpecId :: Int
    } deriving (Show)

data TrtVideoSNPT = TrtVideoSNPT {
    tVideoSNPTUrls :: String
    } deriving (Show)

data TrtVideoSNPTUrl = TrtVideoSNPTUrl String

instance FromJSON TrtVideoInfo where
    parseJSON (Object v) = TrtVideoInfo <$> v .: "res_list" <*> v .: "ret"

instance FromJSON TrtVideoRsp where
    parseJSON (Object v) = TrtVideoRsp <$> v .: "gcid" <*> v .: "snpt_list"
   			  <*> v .: "specid"

instance FromJSON TrtVideoSNPT where
    parseJSON (Object v) = TrtVideoSNPT <$> v .: "snpt_url"
