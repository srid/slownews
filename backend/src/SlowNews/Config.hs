{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module SlowNews.Config where

import           Data.Aeson           (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy as B
import           Data.Maybe           (fromJust)
import           GHC.Generics         (Generic)
import qualified SlowNews.HackerNews  as HackerNews
import qualified SlowNews.Reddit      as Reddit
import           SlowNews.Site        (Site)

data Config = Config
  { sites :: [Site]
  } deriving (Show, Eq, Generic)

instance FromJSON Config
instance ToJSON Config

load :: IO Config
load = fromJust . decode <$> B.readFile "config/config.json"

loadSites :: IO [Site]
loadSites = sites <$> load
