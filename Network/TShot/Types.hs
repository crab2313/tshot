module Network.TShot.Types where

type HashCode = String
type Link = String
type VideoId = Int

-- Torrent:
data Torrent = Torrent {
      tVideo :: [Video],
      tHash  :: HashCode
-- record number ? (avaliable)
    } deriving (Show)

-- Video
data Video = Video {
      videoId     :: VideoId,
      videoName   :: String,
      videoThumbs :: [Thumbnail]
-- size ? 
--
    } deriving (Show)

-- toss it ? I think we just need a list of Strings.
-- or reserve it for future purpose?
data Thumbnail = Thumbnail {
      tbLink :: Link
    } deriving (Show)
