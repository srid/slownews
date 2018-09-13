{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Data.Function (on)
import Data.List (sortBy)
import Data.Semigroup ((<>))
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

import Obelisk.Frontend
import Obelisk.Route.Frontend
import Reflex.Dom.Core hiding (Link)

import Static

import Common.Link (Link (..))
import Common.Route

import Frontend.CSS (appCssStr)
import Frontend.ReflexUtil (getAndDecodeWithError, matchEither, matchMaybe)

-- TODO: Rename Link type; conflicts with other modules.
type CurrentLinks = Maybe (Either String [Link])

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
  , _frontend_body = subRoute_ $ \_r -> divClass "ui container" $ do
      elClass "h1" "ui top attached inverted header" $ text "SlowNews"
      divClass "ui attached segment" $ do
        divClass "content" $ do
          viewLinks =<< prerender (pure $ constDyn Nothing) getLinks
      divClass "ui bottom attached secondary segment" $ do
        elAttr "a" ("href" =: "https://github.com/srid/slownews") $ do
          text "SlowNews on GitHub (powered by Haskell and Reflex)"

  , _frontend_notFoundRoute = \_ -> Route_Home :/ () -- TODO: not used i think
  }

viewLinks :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m) => Dynamic t CurrentLinks -> m ()
viewLinks links'' = matchMaybe links'' $ \case
  Nothing -> divClass "ui active dimmer" $ divClass "ui loader" blank
  Just links' -> matchEither links' $ \case
    Left err -> dynText $ T.pack <$> err
    Right links -> void $ simpleList (sortLinks <$> links) viewLink
  where
    sortLinks = sortBy (flip compare `on` linkCreated)

viewLink :: (DomBuilder t m, PostBuild t m) => Dynamic t Link -> m ()
viewLink dLink = divClass "ui three column grid" $ divClass "row" $ do
  divClass "one wide column" $ do
    dynText $ dayOfWeek . linkCreated <$> dLink
  divClass "five wide column meta" $ do
    dynA (linkMetaUrl <$> dLink) (linkSite <$> dLink)
  divClass "nine wide column" $ do
    dynA (linkUrl <$> dLink) (linkTitle <$> dLink)
  where
    dayOfWeek = T.pack . formatTime defaultTimeLocale "%a" . posixSecondsToUTCTime . fromIntegral

-- | Like dynText but for <a href...
dynA :: (DomBuilder t m, PostBuild t m) => Dynamic t T.Text -> Dynamic t T.Text -> m ()
dynA url title = elDynAttr "a" dAttr $ dynText title
  where dAttr = ffor url $ \u -> "href" =: u <> "target" =: "_blank"

getBaseUrl :: Monad m => m Text
getBaseUrl = do
  -- FIXME: change this after fixing the backend
  -- We need to inject from Obelisk.
  -- cf. https://github.com/obsidiansystems/obelisk/pull/91
  -- pure "https://slownews.srid.ca"
  pure ""

-- | Fetch links from the server
getLinks :: MonadWidget t m => m (Dynamic t CurrentLinks)
getLinks = do
  baseUrl <- getBaseUrl
  pb <- getPostBuild
  let urlEvent = baseUrl <> "/data" <$ pb
  resp <- getAndDecodeWithError urlEvent
  holdDyn Nothing $ Just <$> resp
