{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Data.Text                     (Text)
import           GHC.Generics
import           Network.Wai.Middleware.Static
import qualified Network.Wreq                  as WQ
import           Web.Scotty

import Reddit


main :: IO ()
main = do
  posts       <- atomically $ newTVar ([] :: [Reddit.Post])
  sample_body <- sampleBody
  atomically $ writeTVar posts (Reddit.children sample_body)

  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "../frontend/static")
    get "/" $ redirect "/index.html" -- TODO: Hide index.html from address bar.
    get "/data" $ do
      currentPosts <- liftIO $ atomically $ readTVar posts
      json currentPosts
