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
    -- https://hn.algolia.com/api
    -- TODO: process data text.
    link <- Link <$> d .: "title"
                 <*> d .: "url"
                 <*> d .: "objectID"
                 <*> d .: "created_at_i"
                 <*> d .: "author" -- Dummy parsing
    return link {
        linkMetaUrl = T.pack "https://news.ycombinator.com/item?id=" `T.append` linkMetaUrl link
      , linkSite = "HN"
    }

fetch :: Maybe String -> Maybe Int -> IO [Link]
fetch queryMaybe countMaybe = do
  r <- asJSON =<< getWith opts url :: IO (Response Body)
  return $ fixLink <$> (r ^. responseBody & bodyChildren)
  where
    url = "http://hn.algolia.com/api/v1/search"
    count = fromMaybe 10 countMaybe
    query = fromMaybe "" queryMaybe 
    opts = defaults & params .~
              [ ("tags", "story")
              -- , ("numericFilters", "created_at_i>#{one_week_ago}") TODO
              , ("hitsPerPage", T.pack . show  $ count)
              , ("query", T.pack . show $ query) 
              ]
    fixLink link = link { linkSite = linkSite link `T.append` T.pack "/" `T.append` T.pack query }