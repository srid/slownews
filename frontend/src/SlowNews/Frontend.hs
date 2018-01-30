{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- 1. DONE Write basic UI
-- 2. DONE Shell + normal mode
-- 2. TODO XHR
-- 3. TODO Complete UI

import Data.FileEmbed

import SlowNews.Link (Link (..))

-- Temporary fix to address mismatch in `run` declaration between platforms.
#if defined(MIN_VERSION_jsaddle_warp)
import Language.Javascript.JSaddle.Warp (run)
import Language.Javascript.JSaddle (JSM)
import Reflex.Dom hiding (Link, mainWidgetWithCss, run)
import Reflex.Dom.Core (mainWidgetWithCss)
run2 :: JSM () -> IO ()
run2 = run 3001
#else
import Reflex.Dom hiding (Link)
run2 = id
#endif


main :: IO ()
main = run2 $ mainWidgetWithCss css app
  where css = $(embedFile "style.css")

app :: MonadWidget t m => m ()
app = el "div" $ do
  elClass "h1" "title" $ text "SlowNews"
  el "tr" $ do
    simpleList
      (constDyn sampleLinks)
      (\d -> dyn $ displayLink <$> d)
  divClass "footer" $ do
    elAttr "a" ("href" =: "https://github.com/srid/slownews") $ do
      text "SlowNews on GitHub"

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
