{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Backend.Lobsters where

import Common.Link (Link (Link))
import Control.Lens ((^.))
import Data.Aeson ((.:), FromJSON (parseJSON), ToJSON, withObject)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import GHC.Generics (Generic)
import qualified Network.Wreq as WQ

data Site = Site
  { tag :: String,
    count :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON Site

instance ToJSON Site

data Submission = Submission
  { submissionTitle :: Text,
    submissionUrl :: Text,
    submissionCommentsUrl :: Text,
    submissionCreatedAt :: UTCTime,
    submissionScore :: Int
  }
  deriving (Show, Eq)

instance FromJSON Submission where
  parseJSON = withObject "Submission" $ \v -> do
    Submission <$> v .: "title"
      <*> v .: "url"
      <*> v .: "comments_url"
      <*> v .: "created_at"
      <*> v .: "score"

toLink :: Text -> Submission -> Link
toLink t Submission {..} =
  Link submissionTitle submissionUrl submissionCommentsUrl (floor $ utcTimeToPOSIXSeconds submissionCreatedAt) ("lobsters/" <> t)

fetch :: Site -> IO [Link]
fetch Site {..} = do
  r <- WQ.asJSON =<< WQ.get ("https://lobste.rs/t/" <> tag <> ".json") :: IO (WQ.Response [Submission])
  return $ maybe id take count $ fmap (toLink $ T.pack tag) $ r ^. WQ.responseBody
