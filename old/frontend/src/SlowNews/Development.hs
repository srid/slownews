{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module SlowNews.Development
  ( appMain
  , dataUrl
  ) where

import Data.Text (Text)
import qualified Reflex.Dom.Main as Main

import Reflex.Dom hiding (run)
import SlowNews.CSS

#if defined(ghcjs_HOST_OS)
run :: a -> a
run = id

dataUrl :: Text
dataUrl = "/data"
#elif defined(MIN_VERSION_jsaddle_warp)
import Language.Javascript.JSaddle (JSM)
import qualified Language.Javascript.JSaddle.Warp as JW
run :: JSM () -> IO()
run jsm = do
  putStrLn $ "Running jsaddle-warp server on port 3003"
  JW.run 3003 jsm

dataUrl :: Text
dataUrl = "http://localhost:3000/data"
#endif

appMain :: (forall x. Widget x ()) -> IO ()
appMain w = run $ Main.mainWidgetWithCss cssInline w
