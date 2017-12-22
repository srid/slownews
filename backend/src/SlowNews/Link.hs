{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module SlowNews.Link where

import           Data.Aeson        (ToJSON (toJSON), genericToJSON)
import           Data.Aeson.Casing (aesonPrefix, snakeCase)
import           Data.Text         (Text)
import           GHC.Generics      (Generic)

data Link =
  Link { linkTitle   :: Text
       , linkUrl     :: Text
       , linkMetaUrl :: Text
       , linkCreated :: Int
       , linkSite    :: Text
       }
  deriving (Show, Eq, Generic)

instance ToJSON Link where
  toJSON = genericToJSON $ aesonPrefix snakeCase
