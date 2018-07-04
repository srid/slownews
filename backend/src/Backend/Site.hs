{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Site where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid ((<>))
import GHC.Generics (Generic)
import Katip (Severity (InfoS), logTM, showLS)

import qualified Backend.HackerNews as HackerNews
import qualified Backend.Reddit as Reddit
import Backend.Stack (Stack)
import Common.Link (Link)

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
