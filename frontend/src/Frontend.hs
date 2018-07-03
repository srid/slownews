{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Data.Semigroup ((<>))
import qualified Data.Text as T

import Reflex.Dom.Core

import Frontend.App (app)
import Frontend.CSS (cssInline)


frontend :: (StaticWidget x (), Widget x ())
frontend = (head', body)
  where
    head' = do
      el "title" $ text "SlowNews"
      el "style" $ text cssInline
    body = app
