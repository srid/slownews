{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module SlowNews.Site where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid ((<>))
import GHC.Generics (Generic)
import Katip (Severity (InfoS), logTM, showLS)

import qualified SlowNews.HackerNews as HackerNews
import SlowNews.Link (Link)
import qualified SlowNews.Reddit as Reddit
import SlowNews.Stack (Stack)

data Site
  = Reddit Reddit.Site
  | HackerNews HackerNews.Site
  deriving (Show, Eq, Generic)

instance FromJSON Site
instance ToJSON Site

fetchSite :: Site -> Stack [Link]
fetchSite site = do
  $(logTM) InfoS $ "Fetching " <> showLS site
  liftIO $ fetch site
  where
    fetch (Reddit s)     = Reddit.fetch s
    fetch (HackerNews s) = HackerNews.fetch s
