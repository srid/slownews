{-# LANGUAGE OverloadedStrings #-}

module SlowNews.HackerNews where

import           Control.Lens ((.~), (^.), (&))
import           Data.Aeson                    (FromJSON (..),
                                                withObject, (.:))
import Data.Maybe (fromMaybe)
import Network.Wreq  (defaults, params, getWith, asJSON, responseBody, Response)                
import SlowNews.Link (Link(..))
import qualified Data.Text as T

data Body =
  Body { bodyChildren :: [Link] }
  deriving (Show, Eq)

instance FromJSON Body where
  parseJSON = withObject "Body" $ \d -> do
    Body <$> d .: "hits"

-- TODO: avoid orphan instances
instance FromJSON Link where
  parseJSON = withObject "Link" $ \d -> do
    -- TODO: process data text.
    Link <$> d .: "title"
         <*> d .: "url"
         <*> d .: "objectID"
         <*> d .: "created_at_i"
         <*> d .: "author"

fetch :: Maybe Int -> IO [Link]
fetch countMaybe = do
  let url = "http://hn.algolia.com/api/v1/search"
  let count = fromMaybe 10 countMaybe
  let query = "" -- TODO
  let opts = defaults & params .~
             [ ("tags", "story")
             -- , ("numericFilters", "created_at_i>#{one_week_ago}") TODO
             , ("hitsPerPage", T.pack . show  $ count)
             , ("query", T.pack . show $ query) ]
  
  r <- asJSON =<< getWith opts url :: IO (Response Body)
  return $ r ^. responseBody & bodyChildren