module TShot.Type where

type HashCode = String
type Link = String
type VideoID = Int


data Torrent = Torrent {
      tVideo :: [Video],
      tHash  :: HashCode
    }

data Video = Video {
      videoID     :: VideoID,
      videoName   :: String,
      videoThumbs :: [Thumbnail]
    }

data Thumbnail = Thumbnail {
      tbLinks :: [Link]
    }