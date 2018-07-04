{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backend where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import Data.Default (Default (..))
import Data.Text (Text)
import System.IO (BufferMode (..), hSetBuffering, stderr)

import Reflex.Dom (renderStatic)

import Snap (Snap, commandLineConfig, defaultConfig, httpServe, route, writeBS)
import Snap.Internal.Http.Server.Config (Config (accessLog, errorLog), ConfigLog (ConfigIoLog))

import Obelisk.Asset.Serve.Snap (serveAssets)
import qualified Obelisk.Backend as Ob
import Obelisk.Snap

import Frontend

import Backend.Entrypoint (start)

backend :: IO ()
backend = do
  getLinks <- start
  obBackend ("/data", serveData getLinks) cfg
  where
    cfg = Ob.def
      { Ob._backendConfig_head = fst frontend
      }
    serveData getLinks = do
      links <- liftIO getLinks
      writeBS $ BSL.toStrict $ encode links


-- | Fork of Ob.backend with a first argument that customizes it.
--
-- Eventually this function should go away as Obelisk progresses.
obBackend :: (BSC8.ByteString, Snap ()) -> Ob.BackendConfig -> IO ()
obBackend apiRoute cfg = do
  -- Make output more legible by decreasing the likelihood of output from
  -- multiple threads being interleaved
  hSetBuffering stderr LineBuffering

  -- Get the web server configuration from the command line
  cmdLineConf <- commandLineConfig defaultConfig
  headHtml <- fmap snd $ renderStatic $ Ob._backendConfig_head cfg
  let httpConf = cmdLineConf
        { accessLog = Just $ ConfigIoLog BSC8.putStrLn
        , errorLog = Just $ ConfigIoLog BSC8.putStrLn
        }
      appCfg = def & appConfig_initialHead .~ headHtml
  -- Start the web server
  httpServe httpConf $ route
    [ apiRoute
    , ("", serveApp "" appCfg)
    , ("", serveAssets "frontend.jsexe.assets" "frontend.jsexe") --TODO: Can we prevent naming conflicts between frontend.jsexe and static?
    , ("", serveAssets "static.assets" "static")
    ]
