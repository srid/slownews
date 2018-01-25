{-# LANGUAGE OverloadedStrings #-}

import qualified Language.Javascript.JSaddle.Warp as JSaddle
import           Reflex.Dom                       hiding (mainWidget)
import           Reflex.Dom.Core                  (mainWidget)

main :: IO ()
main = JSaddle.run 3001 app

app = mainWidget $ el "div" $ do
 text "SlowNews Port in development [reflex-port]"
