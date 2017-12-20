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
    | HackerNews { query :: Maybe String 
                 , count :: Maybe Int }
    deriving (Show, Eq, Generic)

instance FromJSON Config
instance FromJSON Site

load :: IO Config
load = fromJust . decode <$> B.readFile "config.json"

loadSites :: IO [Site]
loadSites = load >>= return . sites