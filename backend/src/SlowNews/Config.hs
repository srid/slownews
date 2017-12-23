{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module SlowNews.Config where

import           Data.Aeson           (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy as B
import           Data.Maybe           (fromJust)
import           GHC.Generics         (Generic)
import qualified SlowNews.HackerNews  as HackerNews
import qualified SlowNews.Reddit      as Reddit

data Config = Config
  { sites :: [Site]
  } deriving (Show, Eq, Generic)

data Site
  = Reddit Reddit.Site
  | HackerNews HackerNews.Site
  deriving (Show, Eq, Generic)

instance FromJSON Config
instance FromJSON Site
instance ToJSON Config
instance ToJSON Site

load :: IO Config
load = fromJust . decode <$> B.readFile "config.json"

loadSites :: IO [Site]
loadSites = sites <$> load
