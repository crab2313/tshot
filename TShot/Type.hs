module TShot.Type where

type HashCode = String
type Link = String
type VideoID = Int

-- Torrent:
data Torrent = Torrent {
      tVideo :: [Video],
      tHash  :: HashCode
    } deriving (Show)

torrentFromHash :: HashCode -> Torrent
torrentFromHash = undefined


-- Video
data Video = Video {
      videoID     :: VideoID,
      videoName   :: String,
      videoThumbs :: [Thumbnail]
    } deriving (Show)

data Thumbnail = Thumbnail {
      tbLinks :: Link
    } deriving (Show)
