{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module SlowNews.Config where

import           GHC.Generics
import           Data.Maybe (fromJust)
import           Data.Aeson   (decode, FromJSON(..))
import qualified Data.ByteString.Lazy as B

data Config =
    Config { sites :: [Site] }
    deriving (Show, Eq, Generic)

data Site
    = Reddit { subReddit :: String
             , count :: Maybe Int }
    | HackerNews
    deriving (Show, Eq, Generic)

instance FromJSON Config
instance FromJSON Site

load :: IO Config
load = fromJust . decode <$> B.readFile "config.json"