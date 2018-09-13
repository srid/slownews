{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Data.Semigroup ((<>))

import Obelisk.Frontend
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Static

import Common.Route

import Frontend.App (app)
import Frontend.CSS (appCssStr)

frontend :: Frontend (R Route)
frontend = Frontend
  { _frontend_head = do
      elAttr "base" ("href" =: "/") blank
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
      el "style" $ text appCssStr
      el "title" $ do
        r <- askRoute
        dynText $ ffor r $ \case
          (_ :: R Route) -> "SlowNews" -- Placeholder for title changing logic
      elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: static @"semantic.min.css") blank
  , _frontend_body = subRoute_ $ \_r -> do
      -- TODO: Use prerender at low level
      prerender (text "Loading...") app
  , _frontend_notFoundRoute = \_ -> Route_Home :/ () -- TODO: not used i think
  }
