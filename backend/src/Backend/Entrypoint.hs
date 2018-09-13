{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Entrypoint where
import Control.Concurrent.Async.Lifted (mapConcurrently)
import Control.Concurrent.Lifted (fork, threadDelay)
import Control.Concurrent.STM (TVar, atomically, newTVar, readTVar, writeTVar)
import Control.Exception.Safe (handle)
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
handleHttpException e = do
  putStrLn $ "ERROR: Http exception:" <> show e
  return ()

fetchAll' :: App.App -> Links -> IO ()
fetchAll' app links = handle handleHttpException (fetchAll app links)

start :: IO (IO [Link])
start = do
  -- TODO: use a logging library instead of buffering stdout for saner putStrLn
  -- behaviour.
  hSetBuffering stdout LineBuffering

  app <- App.makeApp
  print app
  links <- atomically $ newTVar mempty
  void $ fork $ forever (fetchAll' app links >> sleepM 30)
  pure $ atomically $ readTVar links
  where
    sleepM n = threadDelay (n * 60 * 1000 * 1000) -- sleep in minutes
