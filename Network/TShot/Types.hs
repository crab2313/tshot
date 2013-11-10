module Network.TShot.Types where

type HashCode = String
type Link = String
type VideoId = Int

-- Torrent:
data Torrent = Torrent {
    trtHash  :: HashCode,
    trtVideo :: [Maybe Video]
-- record number ? (avaliable)
    } deriving (Show)

-- Video
data Video = Video {
    videoId     :: VideoId,
    videoName   :: String,
    videoThumbs :: [String],
    videoSize :: Int
    } deriving (Show)
