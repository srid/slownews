{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module SlowNews.Reddit where

import           Control.Lens
import           Data.Aeson                    (FromJSON (..), ToJSON,
                                                withObject, (.:))
import           Data.Text                     (Text)
import           GHC.Generics
import qualified Network.Wreq                  as WQ
import SlowNews.Link (Link(..))

data Body =
  Body { children :: [Link] }
  deriving (Show, Eq)

instance FromJSON Body where
  parseJSON = withObject "Body" $ \v -> do
    d <- v .: "data"
    Body <$> d .: "children"

instance FromJSON Link where
  parseJSON = withObject "Link" $ \v -> do
    d <- v .: "data"
    Link <$> d .: "title"
         <*> d .: "url"
         <*> d .: "permalink"
         <*> d .: "created_utc"
         <*> d .: "subreddit_name_prefixed"

sampleBody :: IO Body
sampleBody = do
  let
    sample_url =
      "https://www.reddit.com/r/programming/top/.json?sort=top&t=week&limit=10"
  r <- WQ.asJSON =<< WQ.get sample_url :: IO (WQ.Response Body)
  return $ r ^. WQ.responseBody
