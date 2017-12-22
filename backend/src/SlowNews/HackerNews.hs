{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module SlowNews.HackerNews where

import           Control.Lens          ((&), (.~), (^.))
import           Data.Aeson            (FromJSON (parseJSON), withObject, (.:))
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Time             (UTCTime (UTCTime), addDays,
                                        getCurrentTime, utctDay)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Network.Wreq          (Response, asJSON, defaults, getWith,
                                        params, responseBody)
import           SlowNews.Link         (Link (Link))

data Body =
  Body { bodyChildren :: [HNLink] }
  deriving (Show, Eq)

instance FromJSON Body where
  parseJSON = withObject "Body" $ \d -> Body <$> d .: "hits"

data HNLink =
  HNLink { hnlinkTitle      :: Text
         , hnlinkUrl        :: Text
         , hnlinkObjectID   :: Text
         , hnlinkCreatedAtI :: Int
         }
  deriving (Show, Eq)

instance FromJSON HNLink where
  parseJSON = withObject "HNLink" $ \d ->
    HNLink <$> d .: "title"
           <*> d .: "url"
           <*> d .: "objectID"
           <*> d .: "created_at_i"

toLink :: HNLink -> Link
toLink HNLink{hnlinkTitle, hnlinkUrl, hnlinkObjectID, hnlinkCreatedAtI} =
  Link hnlinkTitle hnlinkUrl metaURL hnlinkCreatedAtI siteName
  where metaURL = "https://news.ycombinator.com/item?id=" <> hnlinkObjectID
        siteName = "hn"

fetch :: Maybe String -> Maybe Int -> IO [Link]
fetch queryMaybe countMaybe = do
  created_at_i <- show <$> (oneWeekAgo :: IO Integer)
  r <- asJSON =<< getWith (opts created_at_i) url :: IO (Response Body)
  return $ fmap toLink $ r ^. responseBody & bodyChildren
  where
    url = "http://hn.algolia.com/api/v1/search"
    count = fromMaybe 10 countMaybe
    query = fromMaybe "" queryMaybe
    oneWeekAgo = toTimestamp . addDays (-7) <$> now
      where now = utctDay <$> getCurrentTime
            toTime day = UTCTime day 0
            toTimestamp = round . utcTimeToPOSIXSeconds . toTime
    opts c = defaults & params .~
              [ ("tags", "story")
              , ("numericFilters", T.pack $ "created_at_i>" ++ c)
              , ("hitsPerPage", T.pack . show  $ count)
              , ("query", T.pack . show $ query)
              ]
