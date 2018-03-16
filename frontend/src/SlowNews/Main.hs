{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (void)
import Data.Function (on)
import Data.List (sortBy)
import Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Reflex.Dom hiding (Link)

import Reflex.Dom.SemanticUI hiding (Link)

import SlowNews.Link (Link (..))
import SlowNews.Native
import SlowNews.ReflexUtil

-- TODO: Rename Link type; conflicts with other modules.
type CurrentLinks = Maybe (Either String [Link])

main :: IO ()
main = appMain app

app :: MonadWidget t m => m ()
app = container def $ do
  links'' <- getLinks
  segment def $ do
    header def $ do
      el "h1" $ text appTitle
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

-- | Fetch links from the server
getLinks :: MonadWidget t m => m (Dynamic t CurrentLinks)
getLinks = do
  pb <- getPostBuild
  let urlEvent = "/data" <$ pb
  resp :: Event t (Either String [Link]) <- getAndDecodeWithError urlEvent
  holdDyn Nothing $ Just <$> resp
