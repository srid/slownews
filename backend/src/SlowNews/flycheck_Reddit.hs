{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Reddit where

import           Control.Lens
import           Data.Aeson                    (FromJSON (..), ToJSON,
                                                withObject, (.:))
import           Data.Text                     (Text)
import           GHC.Generics
import qualified Network.Wreq                  as WQ

data Body =
  Body { children :: [Post] }
  deriving (Show, Eq)

instance FromJSON Body where
  parseJSON = withObject "Body" $ \v -> do
    d <- v .: "data"
    Body <$> d .: "children"

data Post =
  Post { title    :: Text
       , url      :: Text
       , meta_url :: Text
       , created  :: Int
       , site     :: Text
       }
  deriving (Show, Eq, Generic)

instance FromJSON Post where
  parseJSON = withObject "Post" $ \v -> do
    d <- v .: "data"
    Post <$> d .: "title"
         <*> d .: "url"
         <*> d .: "permalink"
         <*> d .: "created_utc"
         <*> d .: "subreddit_name_prefixed"

instance ToJSON Post

sampleBody :: IO Body
sampleBody = do
  let
    sample_url =
      "https://www.reddit.com/r/programming/top/.json?sort=top&t=week&limit=10"
  r <- WQ.asJSON =<< WQ.get sample_url :: IO (WQ.Response Body)
  return $ r ^. WQ.responseBody
