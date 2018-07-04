{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend.App where

import Control.Monad (void)
import Data.Function (on)
import Data.List (sortBy)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

import Reflex.Dom.Core hiding (Link)
import Reflex.Dom.SemanticUI hiding (Link, mainWidgetWithCss)

import Common.Link (Link (..))
import Frontend.ReflexUtil

-- TODO: Rename Link type; conflicts with other modules.
type CurrentLinks = Maybe (Either String [Link])

app :: MonadWidget t m => m ()
app = container def $ do
  links'' <- getLinks
  segment def $ do
    header def $ do
      el "h1" $ text "SlowNews"
    divClass "content" $ do
      currentLinks links''
  divClass "footer" $ do
    elAttr "a" ("href" =: "https://github.com/srid/slownews") $ do
      text "SlowNews on GitHub (powered by Haskell and Reflex)"

currentLinks :: MonadWidget t m
             => Dynamic t CurrentLinks
             -> m ()
currentLinks links'' = divClass "ui very basic table" $ matchMaybe links''
  (\case
      Nothing -> divClass "ui text loader" $ text "Loading..."
      Just links' -> matchEither links'
        (\case
            (Left err) -> dynText $ T.pack <$> err
            (Right links) ->
              void $ simpleList (sortLinks <$> links) displayLink))
  where
    sortLinks = sortBy (flip compare `on` linkCreated)

displayLink :: MonadWidget t m => Dynamic t Link -> m ()
displayLink dLink = el "tr" $ do
  el "td" $ do
    dynText (dayOfWeek . linkCreated <$> dLink)
  elClass "td" "meta" $ do
    dynA (linkMetaUrl <$> dLink) (linkSite <$> dLink)
  el "td" $ do
    dynA (linkUrl <$> dLink) (linkTitle <$> dLink)
  where
    dayOfWeek = T.pack . formatTime defaultTimeLocale "%a" . posixSecondsToUTCTime . fromIntegral

-- | Like dynText but for <a href...
dynA :: MonadWidget t m => Dynamic t T.Text -> Dynamic t T.Text -> m ()
dynA url title = elDynAttr "a" dAttr $ dynText title
  where dAttr = ffor url $ \u -> "href" =: u

getBaseUrl :: Monad m => m Text
getBaseUrl = do
  -- FIXME: change this after fixing the backend
  -- We need to inject from Obelisk.
  -- cf. https://github.com/obsidiansystems/obelisk/pull/91
  pure "https://slownews.srid.ca"
  -- pure "http://localhost:3001" (need backend routes!)

-- | Fetch links from the server
getLinks :: MonadWidget t m => m (Dynamic t CurrentLinks)
getLinks = do
  baseUrl <- getBaseUrl
  pb <- getPostBuild
  let urlEvent = baseUrl <> "/data" <$ pb
  resp :: Event t (Either String [Link]) <- getAndDecodeWithError urlEvent
  holdDyn Nothing $ Just <$> resp
