{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Data.Aeson                    (FromJSON (..), ToJSON,
                                                withObject, (.:))
import           Data.Text                     (Text)
import           GHC.Generics
import           Network.Wai.Middleware.Static
import qualified Network.Wreq                  as WQ
import           Web.Scotty


type Resp = WQ.Response Body

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
  r <- WQ.asJSON =<< WQ.get sample_url :: IO Resp
  return $ r ^. WQ.responseBody

main :: IO ()
main = do
  posts <- atomically $ newTVar ([] :: [Post])
  sample_body <- sampleBody
  atomically $ writeTVar posts (Main.children sample_body)

  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "../frontend/static")
    get "/" $ 
      redirect "/index.html"  -- TODO: Hide index.html from address bar.
    get "/data" $ do 
      currentPosts <- liftIO $ atomically $ readTVar posts
      json currentPosts
