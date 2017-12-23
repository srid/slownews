{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts  #-}

import           Control.Concurrent.Async.Lifted      (mapConcurrently)
import           Control.Concurrent.Lifted            (fork, threadDelay)
import           Control.Concurrent.STM               (TVar, atomically,
                                                       newTVar, readTVar,
                                                       writeTVar)
import           Control.Exception
import           Control.Monad                        (forever, join)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Reader
import           Data.IORef
import           Katip
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static        (addBase, noDots,
                                                       staticPolicy, (>->))
import           System.IO                            (stdout)
import           Web.Scotty                           (get, json, middleware,
                                                       redirect, scotty)

import qualified SlowNews.Config                      as Config
import qualified SlowNews.HackerNews                  as HN
import           SlowNews.Link                        (Link)
import qualified SlowNews.Reddit                      as Reddit

type Stack a = ReaderT (IORef Int) (KatipContextT IO) a

type Links = TVar [Link]

fetchSite :: Config.Site -> Stack [Link]
fetchSite site = do
  $(logTM) InfoS $ "Fetching " -- ++ show site
  liftIO $ fetch site
  where
    fetch (Config.Reddit s)     = Reddit.fetch s
    fetch (Config.HackerNews s) = HN.fetch s

fetchAll :: Links -> Stack ()
fetchAll links = do
  sites <- liftIO Config.loadSites
  results <- fetchSites sites
  liftIO $ storeTVar links results
  $(logTM) InfoS "Finished"
  where
    fetchSites = fmap join . mapConcurrently fetchSite
    storeTVar tvar = atomically . writeTVar tvar

main :: IO ()
main = do
  ref <- newIORef 0
  handleScribe <- mkHandleScribe ColorIfTerminal stdout InfoS V2
  let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "SlowNews" "development"

  bracket mkLogEnv closeScribes $ \le -> do
    runKatipContextT le (mempty :: LogContexts) mempty $ runReaderT main_ ref

main_ :: Stack()
main_ = do
  links <- liftIO $ atomically $ newTVar mempty
  _ <- fork $ forever (fetchAll links >> sleepM 30)
  -- Run the web server
  liftIO $ scotty 3000 $ do
    middleware $ logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "../frontend/static")
    get "/" $ redirect "/index.html" -- TODO: Hide index.html from address bar.
    get "/data" $ liftTVar links >>= json
  where
    liftTVar = liftIO . atomically . readTVar
    sleepM n = threadDelay (n * 60 * 1000 * 1000) -- sleep in minutes
