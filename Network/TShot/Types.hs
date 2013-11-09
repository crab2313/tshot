module Network.TShot.Types where

type HashCode = String
type Link = String
type VideoID = Int

-- Torrent:
data Torrent = Torrent {
      tVideo :: [Video],
      tHash  :: HashCode
    } deriving (Show)

-- Video
data Video = Video {
      videoID     :: VideoID,
      videoName   :: String,
      videoThumbs :: [Thumbnail]
    } deriving (Show)

data Thumbnail = Thumbnail {
      tbLink :: Link
    } deriving (Show)
