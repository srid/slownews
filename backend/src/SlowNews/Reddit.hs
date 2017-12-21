{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module SlowNews.Reddit where

import           Data.Monoid
import           Control.Lens
import Data.Text (Text)
import           Data.Aeson                    (FromJSON (..),
                                                withObject, (.:))
import qualified Network.Wreq                  as WQ
import SlowNews.Link (Link(..), Linky(..))

data Body =
  Body { bodyChildren :: [RLink] }
  deriving (Show, Eq)

instance FromJSON Body where
  parseJSON = withObject "Body" $ \v -> do
    d <- v .: "data"
    Body <$> d .: "children"

data RLink = RLink 
  { rlinkTitle :: Text 
  , rlinkUrl :: Text
  , rlinkPermalink :: Text
  , rlinkCreatedUtc :: Int 
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

instance Linky RLink where
  toLink RLink{ rlinkTitle, rlinkUrl, rlinkPermalink, rlinkCreatedUtc, rlinkSubredditNamePrefixed } = 
    Link rlinkTitle rlinkUrl metaUrl rlinkCreatedUtc rlinkSubredditNamePrefixed
    where metaUrl = "https://reddit.com" <> rlinkPermalink
 
fetchSubreddit :: String -> Maybe Int -> IO [RLink]
fetchSubreddit subreddit countMaybe = do
  r <- WQ.asJSON =<< WQ.get (url countMaybe) :: IO (WQ.Response Body)
  return $ r ^. WQ.responseBody & bodyChildren
  where
    url Nothing =
      "https://www.reddit.com/r/" ++ subreddit ++ "/top/.json?sort=top&t=week"
    url (Just count) =
      url Nothing ++ "&limit=" ++ show count
