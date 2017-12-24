{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Concurrent.Async.Lifted      (mapConcurrently)
import           Control.Concurrent.Lifted            (fork, threadDelay)
import           Control.Concurrent.STM               (TVar, atomically,
                                                       newTVar, readTVar,
                                                       writeTVar)
import           Control.Exception                    (bracket)
import           Control.Monad                        (forever, join)
import           Control.Monad.IO.Class               (liftIO)
import           Data.Monoid                          ((<>))
import           GHC.Generics                         (Generic)
import           Katip                                (ColorStrategy (ColorIfTerminal),
                                                       KatipContextT,
                                                       LogContexts,
                                                       Severity (ErrorS, InfoS),
                                                       Verbosity (V2),
                                                       closeScribes,
                                                       defaultScribeSettings,
                                                       initLogEnv, logTM, ls,
                                                       mkHandleScribe,
                                                       registerScribe,
                                                       runKatipContextT, showLS)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static        (addBase, noDots,
                                                       staticPolicy, (>->))
import           System.Envy                          (DefConfig (defConfig),
                                                       FromEnv, decodeEnv)
import           System.IO                            (stdout)
import           Web.Scotty                           (get, json, middleware,
                                                       redirect, scotty)

import qualified SlowNews.Config                      as Config
import qualified SlowNews.HackerNews                  as HN
import           SlowNews.Link                        (Link)
import qualified SlowNews.Reddit                      as Reddit

type Stack a = KatipContextT IO a

type Links = TVar [Link]

data AppEnv = AppEnv
  { port :: Int -- "PORT"
  } deriving (Generic, Show)

instance DefConfig AppEnv where
  defConfig = AppEnv 3000

instance FromEnv AppEnv

fetchSite :: Config.Site -> Stack [Link]
fetchSite site = do
  $(logTM) InfoS $ "Fetching " <> showLS site
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
  handleScribe <- mkHandleScribe ColorIfTerminal stdout InfoS V2
  let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "SlowNews" "development"

  bracket mkLogEnv closeScribes $ \le ->
    runKatipContextT le (mempty :: LogContexts) mempty main_

main_ :: Stack()
main_ = do
  appEnvE <- liftIO (decodeEnv :: IO (Either String AppEnv))
  case appEnvE of
    Left err ->
      $(logTM) ErrorS $ ls ("Error reading env: " <> err)
    Right appEnv -> do
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
