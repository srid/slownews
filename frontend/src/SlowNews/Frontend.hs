{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

import Language.Javascript.JSaddle.Warp
import Reflex.Dom hiding (Link, mainWidget, run)
import Reflex.Dom.Core (mainWidget)
import SlowNews.Link (Link (..))

-- TODO
-- 1. Write basic UI
-- 2. XHR
-- 3. Complete UI

main :: IO ()
main = run 3001 $ mainWidget app

app :: MonadWidget t m => m ()
app = el "div" $ do
  elClass "h1" "title" $ text "SlowNews"
  el "ul" $ do
    simpleList
      (constDyn sampleLinks)
      (\d -> dyn $ displayLink <$> d)
  divClass "footer" $ do
    elAttr "a" ("href" =: "https://github.com/srid/slownews") $ do
      text "SlowNews on GitHub"

displayLink :: MonadWidget t m => Link -> m ()
displayLink link_ = do
  el "li" $ do
    elAttr "a" ("href" =: url) $ text title
      where url = linkUrl link_
            title = linkTitle link_

sampleLinks :: [Link]
sampleLinks =
  [ Link "Latest hipster story title" "url" "murl" 0 "siteA"
  , Link "Something politics and boring" "url" "murl" 0 "siteA"
  ]
