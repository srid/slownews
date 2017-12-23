{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent                   (forkIO, threadDelay)
import           Control.Concurrent.Async             (mapConcurrently)
import           Control.Concurrent.STM               (TVar, atomically,
                                                       newTVar, readTVar,
                                                       writeTVar)
import           Control.Monad                        (forever, join)
import           Control.Monad.IO.Class               (liftIO)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static        (addBase, noDots,
                                                       staticPolicy, (>->))
import           Web.Scotty                           (get, json, middleware,
                                                       redirect, scotty)

import qualified SlowNews.Config                      as Config
import qualified SlowNews.HackerNews                  as HN
import           SlowNews.Link                        (Link)
import qualified SlowNews.Reddit                      as Reddit

type Links = TVar [Link]

fetchSite :: Config.Site -> IO [Link]
fetchSite site = do
  putStrLn $ "Fetching " ++ show site -- TODO: Use logging here
  fetch site
  where
    fetch (Config.Reddit s)     = Reddit.fetch s
    fetch (Config.HackerNews s) = HN.fetch s

fetchAll :: Links -> IO ()
fetchAll links = Config.loadSites >>= fetchSites >>= storeTVar links >> logIt
  where
    fetchSites = fmap join . mapConcurrently fetchSite
    storeTVar tvar = atomically . writeTVar tvar
    logIt = putStrLn "Finished."

main :: IO ()
main = do
  links <- atomically $ newTVar mempty
  _ <- forkIO $ forever (fetchAll links >> sleepM 30)
  -- Run the web server
  scotty 3000 $ do
    middleware $ logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "../frontend/static")
    get "/" $ redirect "/index.html" -- TODO: Hide index.html from address bar.
    get "/data" $ liftTVar links >>= json
  where
    liftTVar = liftIO . atomically . readTVar
    sleepM n = threadDelay (n * 60 * 1000 * 1000) -- sleep in minutes
