{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Concurrent.Async (mapConcurrently_)
import           Control.Monad (forever)
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
  putStrLn $ "Fetching " ++ show subReddit -- TODO: Use logging here
  redditLinks <- Reddit.fetchSubreddit subReddit count
  atomically $ appendLinks links redditLinks
fetchSite _ Config.HackerNews = return ()  -- TODO

fetchAll :: Links -> IO ()
fetchAll links = do
  -- Load config file, and fail if it is invalid.
  configB <- B.readFile "config.json"
  let (Just config) = decode configB :: Maybe Config.Config
  let sites = Config.sites config
  -- Fetch all sites asynchronously
  putStrLn $ "Fetching " ++ show (length sites) ++ " sites"
  mapConcurrently_ (forkIO . fetchSite links) sites 
  
fetchAllPeriodically :: Links -> IO ()
fetchAllPeriodically links = forever $ do
  fetchAll links 
  sleepSecs (30 * 60)
  where 
    sleepSecs n = threadDelay (n * 1000 * 1000)

    
main :: IO ()
main = do
  links <- atomically $ newTVar ([] :: [Link])

  _ <- forkIO $ fetchAllPeriodically links
  
  -- Run the web server
  scotty 3000 $ do
    middleware $ logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "../frontend/static")
    get "/" $ redirect "/index.html" -- TODO: Hide index.html from address bar.
    get "/data" $ do
      _links <- liftIO $ atomically $ readTVar links
      json _links
