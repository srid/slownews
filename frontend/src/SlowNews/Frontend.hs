{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- 1. DONE Write basic UI
-- 2. DONE Shell + normal mode
-- 2. TODO XHR
-- 3. TODO Complete UI

import Data.FileEmbed
import Data.Maybe
import Data.Text as T
import Control.Lens
import Control.Monad
import Data.Default
import Data.Monoid
import Reflex.Dom hiding (Link, mainWidgetWithCss)

import SlowNews.Link (Link (..))
import SlowNews.Native

main :: IO ()
main = run2 $ mainWidgetWithCss css app
  where css = $(embedFile "style.css")

app :: MonadWidget t m => m ()
app = el "div" $ do
  elClass "h1" "title" $ text appTitle
  getLinks
  el "tr" $ do
    simpleList
      (constDyn sampleLinks)
      (\d -> dyn $ displayLink <$> d)
  divClass "footer" $ do
    elAttr "a" ("href" =: "https://github.com/srid/slownews") $ do
      text "SlowNews source on GitHub"

-- | Fetch links from the server
getLinks :: MonadWidget t m => m ()
getLinks = do
  pb <- getPostBuild
  let req = xhrRequest "GET" "/data" def
  resp :: Event t (Maybe T.Text) <- fmap (fmap _xhrResponse_responseText) $ performRequestAsync $ req <$ pb
  text "XHR Response => "
  dynText <=< holdDyn "" $ fmapMaybe (id <$>) resp

displayLink :: MonadWidget t m => Link -> m ()
displayLink link_ = do
  el "tr" $ do
    el "td" blank  -- Time
    elClass "td" "meta" blank -- Comments Url
    el "td" $ do
      elAttr "a" ("href" =: url) $ text title
        where url = linkUrl link_
              title = linkTitle link_

sampleLinks :: [Link]
sampleLinks =
  [ Link "Latest hipster story title" "url" "murl" 0 "siteA"
  , Link "Something politics and boring" "url" "murl" 0 "siteA"
  ]
