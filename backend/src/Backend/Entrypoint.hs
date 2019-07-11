{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Entrypoint where
import Control.Concurrent.Async.Lifted (mapConcurrently)
import Control.Concurrent.Lifted (fork, threadDelay)
import Control.Concurrent.STM (TVar, atomically, newTVar, readTVarIO, writeTVar)
import Control.Exception.Safe (catch)
import Control.Monad (forever, join, void)
import Data.Monoid ((<>))
import Network.HTTP.Client (HttpException)
import System.IO (BufferMode (..), hSetBuffering, stdout)

import qualified Backend.App as App
import qualified Backend.Site as Site
import Common.Link (Link)

type Links = TVar [Link]

fetchAll :: App.App -> Links -> IO ()
fetchAll app links = do
  results <- fetchSites sites
  storeTVar links results
  putStrLn "Finished"
  where
    sites = App.sites $ App.config app
    fetchSites = fmap join . mapConcurrently Site.fetchSite
    storeTVar tvar = atomically . writeTVar tvar

handleHttpException :: HttpException -> IO ()
handleHttpException e =
  putStrLn $ "ERROR: Http exception:" <> show e

start :: IO (IO [Link])
start = do
  -- TODO: use a logging library instead of buffering stdout for saner putStrLn
  -- behaviour.
  hSetBuffering stdout LineBuffering

  app <- App.makeApp
  print app
  links <- atomically $ newTVar mempty
  void $ fork $ forever $ do
    fetchAll app links `catch` handleHttpException
    sleepMins 30
  pure $ readTVarIO links
  where
    sleepMins n = threadDelay (n * 60 * 1000 * 1000)
