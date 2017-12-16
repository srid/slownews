{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Network.Wai.Middleware.Static
import           Network.Wai.Middleware.RequestLogger
import           Web.Scotty
import           Data.Aeson (decode)
import qualified Data.ByteString.Lazy as B

import qualified SlowNews.Config as Config
import SlowNews.Reddit as Reddit
import SlowNews.Link (Link, Links, appendLinks)

fetchSite :: Links -> Config.Site -> IO ()
fetchSite links (Config.Reddit subReddit count) = do
  redditLinks <- Reddit.fetchSubreddit subReddit count
  atomically $ appendLinks links redditLinks
fetchSite _ Config.HackerNews = return ()  -- TODO

main :: IO ()
main = do
  links <- atomically $ newTVar ([] :: [Link])

  -- Load config file, and fail if it is invalid.
  configB <- B.readFile "config.json"
  let (Just config) = decode configB :: Maybe Config.Config

  -- Fetch all sites asynchronously
  -- TODO: Do this in a timer
  forM_ (Config.sites config) 
    (forkIO . fetchSite links)

  -- Run the web server
  scotty 3000 $ do
    middleware $ logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "../frontend/static")
    get "/" $ redirect "/index.html" -- TODO: Hide index.html from address bar.
    get "/data" $ do
      _links <- liftIO $ atomically $ readTVar links
      json _links

