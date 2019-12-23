{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Site where

import qualified Backend.HackerNews as HackerNews
import qualified Backend.Reddit as Reddit
import Common.Link (Link)
import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid ((<>))
import GHC.Generics (Generic)

data Site
  = Reddit Reddit.Site
  | HackerNews HackerNews.Site
  deriving (Show, Eq, Generic)

instance FromJSON Site

instance ToJSON Site

fetchSite :: Site -> IO [Link]
fetchSite site = do
  putStrLn $ "Fetching " <> show site
  fetch site
  where
    fetch (Reddit s) = Reddit.fetch s
    fetch (HackerNews s) = HackerNews.fetch s
