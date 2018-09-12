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

import Obelisk.Backend
import Obelisk.Route
import Obelisk.Snap

import Common.Route
import Frontend

import Backend.Entrypoint (start)

backend :: Backend BackendRoute Route
backend = Backend
  { _backend_run = \serve -> do
      getLinks <- liftIO start
      serve $ \_ -> do  -- "/data"
        links <- liftIO getLinks
        writeBS $ BSL.toStrict $ encode links
  , _backend_routeEncoder = backendRouteEncoder
  }
