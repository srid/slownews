{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Concurrent.Async.Lifted      (mapConcurrently)
import           Control.Concurrent.Lifted            (fork, threadDelay)
import           Control.Concurrent.STM               (TVar, atomically,
                                                       newTVar, readTVar,
                                                       writeTVar)
import           Control.Monad                        (forever, join)
import           Control.Monad.IO.Class               (liftIO)
import           Katip                                (Severity (InfoS), logTM,
                                                       showLS)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static        (addBase, noDots,
                                                       staticPolicy, (>->))
import           Web.Scotty                           (get, json, middleware,
                                                       redirect, scotty)

import qualified SlowNews.App                         as App
import           SlowNews.Link                        (Link)
import qualified SlowNews.Site                        as Site
import           SlowNews.Stack                       (Stack)

type Links = TVar [Link]

fetchAll :: App.App -> Links -> Stack ()
fetchAll app links = do
  results <- fetchSites sites
  liftIO $ storeTVar links results
  $(logTM) InfoS "Finished"
  where
    sites = App.sites $ App.config app
    fetchSites = fmap join . mapConcurrently Site.fetchSite
    storeTVar tvar = atomically . writeTVar tvar

main :: IO ()
main = App.runApp $ do
  app <- App.makeApp
  $(logTM) InfoS $ showLS app
  links <- liftIO $ atomically $ newTVar mempty
  _ <- fork $ forever (fetchAll app links >> sleepM 30)
  -- Run the web server
  liftIO $ scotty (port app) $ do
    middleware $ logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "../frontend/static")
    get "/" $ redirect "/index.html"
    get "/data" $ liftTVar links >>= json
  where
    port = App.port . App.env
    liftTVar = liftIO . atomically . readTVar
    sleepM n = threadDelay (n * 60 * 1000 * 1000) -- sleep in minutes
