{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module SlowNews.Config where

import           GHC.Generics
import           Data.Aeson   (ToJSON(..))

data Config =
    Config { sites :: [SiteConfig] }
    deriving (Show, Eq, Generic)

data SiteConfig = 
    SiteConfig { site :: Site
               , count :: Int 
               }
    deriving (Show, Eq, Generic)
    
data Site 
    = Reddit { subReddit :: String }
    -- | HackerNews
    deriving (Show, Eq, Generic)
    
instance ToJSON Config
instance ToJSON SiteConfig
instance ToJSON Site


sample = Config 
    [ SiteConfig (Reddit "zerocarb") 3
    , SiteConfig (Reddit "keto") 12]
