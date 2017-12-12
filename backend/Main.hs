{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ
import Web.Scotty
import Network.Wai.Middleware.Static
import qualified Network.Wreq as WQ
import Control.Lens
import Data.Text (Text)
import Data.Aeson ((.:), Value(..), FromJSON(..))
-- import Data.Aeson.Lens (key)

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
       , permalink :: Text }
  deriving (Show, Eq)

instance FromJSON Post where
  parseJSON (Object o) = do
    d <- o .: "data"
    Post <$> d .: "title"
         <*> d .: "permalink"

-- Use as:
-- b <- sample
-- b
sample = do
  let url = "https://www.reddit.com/r/zerocarb/top/.json?sort=top&t=week&limit=2"
  r <- WQ.asJSON =<< WQ.get url :: IO Resp
  return $ r ^. WQ.responseBody


main :: IO ()
main = scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "../frontend/static")
  get "/" $ do
    redirect "/index.html"  -- TODO: Hide index.html from address bar.
  get "/data" $ do
    text [r|
           [{"url":"https://www.blog.google/topics/next-billion-users/building-india-first-products-and-features/","title":"Google for India: Building India-first products and features","site":"hn/india#max=1","meta_url":"https://news.ycombinator.com/item?id=15851238","created":1512475689},{"url":"https://news.ycombinator.com/item?id=15853374","title":"AMA: NY AG Schneiderman on net neutrality and protecting our voice in government","site":"hn#max=7","meta_url":"https://news.ycombinator.com/item?id=15853374","created":1512494704}]
           |]
