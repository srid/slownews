{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module SlowNews.Config where

import           GHC.Generics
import           Data.Aeson   (FromJSON(..), ToJSON(..))

data Config =
    Config { sites :: [Site] }
    deriving (Show, Eq, Generic)
    
data Site 
    = Reddit { subReddit :: String
             , count :: Maybe Int }
    | HackerNews
    deriving (Show, Eq, Generic)
    
instance ToJSON Config
instance ToJSON Site
instance FromJSON Config
instance FromJSON Site

sample = Config 
    [ Reddit "zerocarb" $ Just 3
    , Reddit "keto" Nothing
    , HackerNews ]
