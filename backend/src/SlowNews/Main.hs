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

import           SlowNews.App                         (AppEnv (port), Stack,
                                                       runApp)
import qualified SlowNews.Config                      as Config
import           SlowNews.Link                        (Link)
import qualified SlowNews.Site                        as Site


type Links = TVar [Link]

fetchAll :: Links -> Stack ()
fetchAll links = do
  sites <- liftIO Config.loadSites
  results <- fetchSites sites
  liftIO $ storeTVar links results
  $(logTM) InfoS "Finished"
  where
    fetchSites = fmap join . mapConcurrently Site.fetchSite
    storeTVar tvar = atomically . writeTVar tvar

main_ :: AppEnv -> Stack()
main_ appEnv = do
  $(logTM) InfoS $ showLS appEnv
  links <- liftIO $ atomically $ newTVar mempty
  _ <- fork $ forever (fetchAll links >> sleepM 30)
  -- Run the web server
  liftIO $ scotty (port appEnv) $ do
    middleware $ logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "../frontend/static")
    get "/" $ redirect "/index.html"
    get "/data" $ liftTVar links >>= json
  where
    liftTVar = liftIO . atomically . readTVar
    sleepM n = threadDelay (n * 60 * 1000 * 1000) -- sleep in minutes

main :: IO ()
main = runApp main_
