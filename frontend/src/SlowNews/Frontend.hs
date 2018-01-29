{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

import Language.Javascript.JSaddle.Warp
import Reflex.Dom hiding (Link, mainWidget, run)
import Reflex.Dom.Core (mainWidget)
import SlowNews.Link (Link (Link))

-- TODO
-- 1. Write basic UI
-- 2. XHR
-- 3. Complete UI

main :: IO ()
main = run 3001 $ mainWidget app

app :: MonadWidget t m => m ()
app = el "div" $ do
  elClass "h1" "title" $ text "SlowNews"
  el "ul" $ simpleList (constDyn sampleLinks) linkUI
  divClass "footer" $ do
    elAttr "a" ("href" =: "https://github.com/srid/slownews") $ do
      text "SlowNews on GitHub"

linkUI :: MonadWidget t m => Dynamic t Link -> m ()
linkUI link_ = do
  elAttr "a" ("href" =: url) $ display link_
    where url = "TODO" -- need to study the dyn html stuff

sampleLinks :: [Link]
sampleLinks = do
  pure $ Link "Link 123" "url" "murl" 0 "siteA"
