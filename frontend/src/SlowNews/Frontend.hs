{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

import           Data.Monoid                      ((<>))
import           Data.Text                        as T
import           Language.Javascript.JSaddle.Warp (run)
import           Reflex.Dom                       hiding (Link, mainWidget, run)
import           Reflex.Dom.Core                  (mainWidget)
import           SlowNews.Link                    (Link (Link))

main :: IO ()
main = run 3001 $ mainWidget app

app :: MonadWidget t m => m ()
app = el "div" $ do
  let link = Link "foo" "url" "mu" 0 "site"
  text $ "SlowNews Port in development [reflex-port]" <> (T.pack . show) link
