{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Network.Wai.Middleware.Static
import           Web.Scotty

import SlowNews.Reddit as Reddit
import SlowNews.Link (Link)


main :: IO ()
main = do
  links       <- atomically $ newTVar ([] :: [Link])
  sample_body <- sampleBody
  atomically $ writeTVar links (Reddit.children sample_body)

  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "../frontend/static")
    get "/" $ redirect "/index.html" -- TODO: Hide index.html from address bar.
    get "/data" $ do
      _links <- liftIO $ atomically $ readTVar links
      json _links
