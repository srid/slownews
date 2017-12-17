{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad (forever, join)
import           Control.Monad.IO.Class (liftIO)
import           Network.Wai.Middleware.Static
import           Network.Wai.Middleware.RequestLogger
import           Web.Scotty

import qualified SlowNews.Config as Config
import SlowNews.Reddit as Reddit
import SlowNews.Link (Link)

type Links = TVar [Link]

fetchSite ::  Config.Site -> IO [Link]
fetchSite (Config.Reddit subReddit count) = do
  putStrLn $ "Fetching " ++ show subReddit -- TODO: Use logging here
  Reddit.fetchSubreddit subReddit count
fetchSite Config.HackerNews = return []  -- TODO

fetchSites :: [Config.Site] -> IO [Link]
fetchSites sites = do 
  putStrLn $ "Fetching " ++ show (length sites) ++ " sites in parallel"
  join <$> mapConcurrently fetchSite sites 

fetchAll :: Links -> IO ()
fetchAll links = do
  config <- Config.load
  -- Fetch all sites asynchronously
  results <- fetchSites $ Config.sites config
  _ <- atomically $ writeTVar links results
  return ()
  
foreverEvery :: Int -> IO () -> IO()
foreverEvery secs action = forever $ do
  action
  sleepSecs secs
  where 
    sleepSecs n = threadDelay (n * 1000 * 1000)

main :: IO ()
main = do
  links <- atomically $ newTVar ([] :: [Link])

  foreverEvery (30 * 60) $ fetchAll links
  
  -- Run the web server
  scotty 3000 $ do
    middleware $ logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "../frontend/static")
    get "/" $ redirect "/index.html" -- TODO: Hide index.html from address bar.
    get "/data" $ do
      _links <- liftIO $ atomically $ readTVar links
      json _links

