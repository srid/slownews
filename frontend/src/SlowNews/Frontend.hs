{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid                      ((<>))
import           Data.Text                        as T
import qualified Language.Javascript.JSaddle.Warp as JSaddle
import           Reflex.Dom                       hiding (Link, mainWidget)
import           Reflex.Dom.Core                  (mainWidget)
import           SlowNews.Link                    (Link (Link))

main :: IO ()
main = JSaddle.run 3001 app

app = mainWidget $ el "div" $ do
  let link = Link "foo" "url" "mu" 0 "site"
  text $ "SlowNews Port in development [reflex-port]" <> (T.pack . show) link
