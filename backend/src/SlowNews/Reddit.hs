{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module SlowNews.Reddit where

import           Control.Lens  ((&), (^.))
import           Data.Aeson    (FromJSON (parseJSON), withObject, (.:))
import           Data.Monoid   ((<>))
import           Data.Text     (Text)
import qualified Network.Wreq  as WQ
import           SlowNews.Link (Link (Link))

data Body =
  Body { bodyChildren :: [RLink] }
  deriving (Show, Eq)

instance FromJSON Body where
  parseJSON = withObject "Body" $ \v -> do
    d <- v .: "data"
    Body <$> d .: "children"

data RLink = RLink
  { rlinkTitle                 :: Text
  , rlinkUrl                   :: Text
  , rlinkPermalink             :: Text
  , rlinkCreatedUtc            :: Int
  , rlinkSubredditNamePrefixed :: Text
  }
  deriving (Show, Eq)

instance FromJSON RLink where
  parseJSON = withObject "RLink" $ \v -> do
    d <- v .: "data"
    RLink <$> d .: "title"
          <*> d .: "url"
          <*> d .: "permalink"
          <*> d .: "created_utc"
          <*> d .: "subreddit_name_prefixed"

toLink :: RLink -> Link
toLink RLink{ rlinkTitle, rlinkUrl, rlinkPermalink, rlinkCreatedUtc, rlinkSubredditNamePrefixed } =
  Link rlinkTitle rlinkUrl metaUrl rlinkCreatedUtc rlinkSubredditNamePrefixed
  where metaUrl = "https://reddit.com" <> rlinkPermalink

fetchSubreddit :: String -> Maybe Int -> IO [Link]
fetchSubreddit subreddit countMaybe = do
  r <- WQ.asJSON =<< WQ.get (url countMaybe) :: IO (WQ.Response Body)
  return $ fmap toLink $ r ^. WQ.responseBody & bodyChildren
  where
    url Nothing =
      "https://www.reddit.com/r/" ++ subreddit ++ "/top/.json?sort=top&t=week"
    url (Just count) =
      url Nothing ++ "&limit=" ++ show count
