{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Data.Semigroup ((<>))

import Reflex.Dom.Core

import Static

import Frontend.App (app)
import Frontend.CSS (appCssStr)

frontend :: (StaticWidget x (), Widget x ())
frontend = (head', body)
  where
    head' = do
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
      el "title" $ text "SlowNews"
      elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: static @"semantic.min.css") blank
      el "style" $ text appCssStr
    body = app
