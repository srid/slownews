{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}

module SlowNews.HackerNews where

import Data.Time
import Data.Monoid
import           Control.Lens ((.~), (^.), (&))
import           Data.Aeson                    (FromJSON (..),
                                                withObject, (.:))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import           GHC.Generics
import Network.Wreq  (defaults, params, getWith, asJSON, responseBody, Response)                
import SlowNews.Link (Link(Link), Linky(..))
import qualified Data.Text as T

data Body =
  Body { bodyChildren :: [HNLink] }
  deriving (Show, Eq)

instance FromJSON Body where
  parseJSON = withObject "Body" $ \d -> Body <$> d .: "hits"

data HNLink =
  HNLink { hnlinkTitle :: Text 
         , hnlinkUrl :: Text 
         , hnlinkObjectID :: Text 
         , hnlinkCreatedAtI :: Int 
         }
  deriving (Show, Eq)

instance FromJSON HNLink where
  parseJSON = withObject "HNLink" $ \d ->
    HNLink <$> d .: "title"
           <*> d .: "url"
           <*> d .: "objectID"
           <*> d .: "created_at_i"

instance Linky HNLink where 
  toLink HNLink{hnlinkTitle, hnlinkUrl, hnlinkObjectID, hnlinkCreatedAtI} =
    Link hnlinkTitle hnlinkUrl metaURL hnlinkCreatedAtI siteName
    where metaURL = "https://news.ycombinator.com/item?id=" <> hnlinkObjectID
          siteName = "hn"
                  
fetch :: Maybe String -> Maybe Int -> IO [HNLink]
fetch queryMaybe countMaybe = do
  created_at_i <- oneWeekAgo
  r <- asJSON =<< getWith (opts created_at_i) url :: IO (Response Body)
  return $ r ^. responseBody & bodyChildren
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