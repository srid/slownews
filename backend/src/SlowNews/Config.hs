{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module SlowNews.Config where

import           Data.Aeson           (FromJSON (..), decode)
import qualified Data.ByteString.Lazy as B
import           Data.Maybe           (fromJust)
import           GHC.Generics

data Config =
    Config { sites :: [Site] }
    deriving (Show, Eq, Generic)

data Site
    = Reddit { subReddit :: String
             , count     :: Maybe Int }
    | HackerNews { query :: Maybe String
                 , count :: Maybe Int }
    deriving (Show, Eq, Generic)

instance FromJSON Config
instance FromJSON Site

load :: IO Config
load = fromJust . decode <$> B.readFile "config.json"

loadSites :: IO [Site]
loadSites = sites <$> load
