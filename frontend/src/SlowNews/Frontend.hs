{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad (void)
import Data.FileEmbed
import Data.Function (on)
import Data.List (sortBy)
import Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Reflex.Dom hiding (Link, mainWidgetWithCss)

import SlowNews.Link (Link (..))
import SlowNews.Native
import SlowNews.ReflexUtil

main :: IO ()
main = run2 $ mainWidgetWithCss css app
  where css = $(embedFile "style.css")

app :: MonadWidget t m => m ()
app = el "div" $ do
  elClass "h1" "title" $ text appTitle
  dLinksE <- getLinks >>= eitherDyn
  el "tr" $ do
    void $ dyn $ ffor dLinksE
      (\case
          Left dError -> dynText $ T.pack <$> dError
          Right dLinks -> void $ simpleList (sortLinks <$> dLinks) displayLink)
  divClass "footer" $ do
    elAttr "a" ("href" =: "https://github.com/srid/slownews") $ do
      text "SlowNews on GitHub (powered by Haskell and Reflex)"

-- | Fetch links from the server
getLinks :: MonadWidget t m => m (Dynamic t (Either String [Link]))
getLinks = do
  pb <- getPostBuild
  let urlEvent = "/data" <$ pb
  resp :: Event t (Either String [Link]) <- getAndDecodeWithError urlEvent
  holdDyn (Left "Loading...") resp

sortLinks :: [Link] -> [Link]
sortLinks = sortBy (flip compare `on` linkCreated)

displayLink :: MonadWidget t m => Dynamic t Link -> m ()
displayLink dLink = el "tr" $ do
  el "td" $ dynText (dayOfWeek . linkCreated <$> dLink)
  elClass "td" "meta" $ dynA (linkMetaUrl <$> dLink) (linkSite <$> dLink)
  el "td" $ dynA (linkUrl <$> dLink) (linkTitle <$> dLink)
    where dayOfWeek = T.pack . formatTime defaultTimeLocale "%a" . posixSecondsToUTCTime . fromIntegral

dynA :: MonadWidget t m => Dynamic t T.Text -> Dynamic t T.Text -> m ()
dynA url title = elDynAttr "a" dAttr $ dynText title
  where dAttr = ffor url $ \u -> "href" =: u
