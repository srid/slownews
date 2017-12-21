{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent                   (forkIO, threadDelay)
import           Control.Concurrent.Async             (mapConcurrently)
import           Control.Concurrent.STM
import           Control.Monad                        (forever, join)
import           Control.Monad.IO.Class               (liftIO)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Web.Scotty

import qualified SlowNews.Config                      as Config
import           SlowNews.HackerNews                  as HN
import           SlowNews.Link                        (Link, toLink)
import           SlowNews.Reddit                      as Reddit

type Links = TVar [Link]

fetchSite :: Config.Site -> IO [Link]
fetchSite (Config.Reddit subReddit count) = do
  putStrLn $ "Fetching " ++ show subReddit -- TODO: Use logging here
  fmap toLink <$> Reddit.fetchSubreddit subReddit count
fetchSite (Config.HackerNews query count) = do
  putStrLn "Fetching HN"
  fmap toLink <$> HN.fetch query count

fetchAll :: Links -> IO ()
fetchAll links = Config.loadSites >>= fetchSites >>= storeTVar links
 where
  fetchSites = fmap join . mapConcurrently fetchSite
  storeTVar tvar = atomically . writeTVar tvar

main :: IO ()
main = do
  links <- atomically $ newTVar mempty

  _     <- forkIO $ forever (fetchAll links >> sleepM 30)

  -- Run the web server
  scotty 3000 $ do
    middleware $ logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "../frontend/static")
    get "/" $
      redirect "/index.html" -- TODO: Hide index.html from address bar.
    get "/data" $
      liftTVar links >>= json
 where
  liftTVar = liftIO . atomically . readTVar
  sleepM n = threadDelay (n * 60 * 1000 * 1000)  -- sleep in minutes
