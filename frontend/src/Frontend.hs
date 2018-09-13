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
import Frontend.ReflexUtil (getAndDecodeWithError)

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
viewLinks links = do
  v <- maybeDyn links
  dyn_ $ ffor v $ \case
    Nothing -> divClass "ui active dimmer" $ divClass "ui loader" blank
    Just links' -> do
      v' <- eitherDyn links'
      divClass "ui tablet stackable selectable inverted table links" $ do
        el "thead" $ do
          el "th" $ text "When"
          el "th" $ text "Source"
          el "th" $ text "Title"
        el "tbody" $ do
          dyn_ $ ffor v' $ \case
            Left err -> dynText $ T.pack <$> err
            Right links'' -> void $ simpleList (sortLinks <$> links'') viewLink
  where
    sortLinks = sortBy (flip compare `on` linkCreated)

viewLink :: (DomBuilder t m, PostBuild t m) => Dynamic t Link -> m ()
viewLink dLink = el "tr" $ do
  elClass "td" "when" $ do
    dynText $ dayOfWeek . linkCreated <$> dLink
  elClass "td" "meta right aligned" $ do
    dynA (linkMetaUrl <$> dLink) (linkSite <$> dLink)
  elClass "td" "title" $ do
    dynA (linkUrl <$> dLink) (linkTitle <$> dLink)
  where
    dayOfWeek = T.pack . formatTime defaultTimeLocale "%a" . posixSecondsToUTCTime . fromIntegral

-- | Like dynText but for <a href...
dynA :: (DomBuilder t m, PostBuild t m) => Dynamic t T.Text -> Dynamic t T.Text -> m ()
dynA url title = elDynAttr "a" dAttr $ dynText title
  where dAttr = ffor url $ \u -> "href" =: u <> "target" =: "_blank"

-- | Fetch links from the server
getLinks :: MonadWidget t m => m (Dynamic t CurrentLinks)
getLinks = do
  pb <- getPostBuild
  resp <- getAndDecodeWithError $ "/data" <$ pb
  holdDyn Nothing $ Just <$> resp
