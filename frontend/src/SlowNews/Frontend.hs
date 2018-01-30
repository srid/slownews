{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Monad
import Data.Default
import Data.FileEmbed
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe
import Data.Monoid
import Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Reflex.Dom hiding (Link, mainWidgetWithCss)

import SlowNews.Link (Link (..))
import SlowNews.Native

main :: IO ()
main = run2 $ mainWidgetWithCss css app
  where css = $(embedFile "style.css")

app :: MonadWidget t m => m ()
app = el "div" $ do
  elClass "h1" "title" $ text appTitle
  linksDyn <- getLinks
  el "tr" $ do
    simpleList (sortLinks <$> linksDyn) displayLink
  divClass "footer" $ do
    elAttr "a" ("href" =: "https://github.com/srid/slownews") $ do
      text "SlowNews source on GitHub"

-- | Fetch links from the server
getLinks :: MonadWidget t m => m (Dynamic t [Link])
getLinks = do
  pb <- getPostBuild
  let urlEvent = "/data" <$ pb
  -- TODO: error handling
  resp :: Event t (Maybe [Link]) <- getAndDecode urlEvent
  holdDyn [loadingLink] $ fmapMaybe (id <$>) resp
    where loadingLink = Link "Loading..." "" "" 0 ""

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
