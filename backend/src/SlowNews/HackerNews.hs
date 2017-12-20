{-# LANGUAGE OverloadedStrings #-}

module SlowNews.HackerNews where

import Data.Time
import Data.Monoid
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
    -- https://hn.algolia.com/api [v1/search] JSON structure
    link <- Link <$> d .: "title"
                 <*> d .: "url"
                 <*> d .: "objectID"
                 <*> d .: "created_at_i"
                 <*> d .: "author" -- Dummy parsing
    return link {
        linkMetaUrl = "https://news.ycombinator.com/item?id=" <> linkMetaUrl link
      , linkSite = "HN"
    }

fetch :: Maybe String -> Maybe Int -> IO [Link]
fetch queryMaybe countMaybe = do
  created_at_i <- oneWeekAgo
  putStrLn $ "create: " ++ created_at_i
  r <- asJSON =<< getWith (opts created_at_i) url :: IO (Response Body)
  return $ fixLink <$> (r ^. responseBody & bodyChildren)
  where
    url = "http://hn.algolia.com/api/v1/search"
    count = fromMaybe 10 countMaybe
    query = fromMaybe "" queryMaybe 
    oneWeekAgo = toTimestamp . toTime . addDays (-7) <$> now
      where now = utctDay <$> getCurrentTime
            toTime day = UTCTime day 0
            toTimestamp = formatTime defaultTimeLocale "%s"
    opts c = defaults & params .~
              [ ("tags", "story")
              , ("numericFilters", T.pack $ "created_at_i>" ++ c)
              , ("hitsPerPage", T.pack . show  $ count)
              , ("query", T.pack . show $ query) 
              ]
    fixLink link = link { linkSite = linkSite link <> "/" <> T.pack query }