{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

import Text.RawString.QQ
import Web.Scotty
import Network.Wai.Middleware.Static
import qualified Network.Wreq as WQ
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Aeson ((.:), Value(..), FromJSON(..), ToJSON)
import GHC.Generics

type Resp = WQ.Response Body

data Body =
  Body { children :: [Post] }
  deriving (Show, Eq)

instance FromJSON Body where
  parseJSON (Object o) = do
    d <- o .: "data"
    Body <$> d .: "children"

data Post =
  Post { title :: Text
       , url :: Text
       , meta_url :: Text
       , created :: Int
       , site :: Text
       }
  deriving (Show, Eq, Generic)

instance FromJSON Post where
  parseJSON (Object o) = do
    d <- o .: "data"
    Post <$> d .: "title"
         <*> d .: "url"
         <*> d .: "permalink"
         <*> d .: "created_utc"
         <*> d .: "subreddit_name_prefixed"

instance ToJSON Post

sample = do
  let url = "https://www.reddit.com/r/programming/top/.json?sort=top&t=week&limit=3"
  r <- WQ.asJSON =<< WQ.get url :: IO Resp
  return $ r ^. WQ.responseBody


main :: IO ()
main = scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "../frontend/static")
  get "/" $ do
    redirect "/index.html"  -- TODO: Hide index.html from address bar.
  get "/data" $ do
    body <- liftIO sample
    json $ Main.children body
