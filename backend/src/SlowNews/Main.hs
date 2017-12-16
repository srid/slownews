{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Network.Wai.Middleware.Static
import           Network.Wai.Middleware.RequestLogger
import           Web.Scotty

import qualified SlowNews.Config as Config
import SlowNews.Reddit as Reddit
import SlowNews.Link (Link, Links, appendLinks)

fetchSite :: Links -> Config.Site -> IO ()
fetchSite links (Config.Reddit subReddit) = do
  redditLinks <- Reddit.fetchSubreddit subReddit
  atomically $ appendLinks links redditLinks

main :: IO ()
main = do
  links <- atomically $ newTVar ([] :: [Link])
  let config = Config.sample
  let sites = Config.site <$> (Config.sites config)
  -- TODO: take 'count' into consideration

  -- Fetch all sites asynchronously
  mapM_ (forkIO . fetchSite links) $ sites

  scotty 3000 $ do
    middleware $ logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "../frontend/static")
    get "/" $ redirect "/index.html" -- TODO: Hide index.html from address bar.
    get "/data" $ do
      _links <- liftIO $ atomically $ readTVar links
      json _links

