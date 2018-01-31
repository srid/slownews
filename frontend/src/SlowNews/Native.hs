{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module SlowNews.Native (appTitle, run2, mainWidgetWithCss) where

import Data.Text as T

-- Temporary fix to address mismatch in `run` declaration between platforms.
#if defined(ghcjs_HOST_OS)
-- We are on ghcjs
import Reflex.Dom hiding (Link)
run2 = id
appTitle :: T.Text
appTitle = "SlowNews"
#elif defined(MIN_VERSION_jsaddle_warp)
-- We are on jsaddle-warp
import Language.Javascript.JSaddle.Warp (run)
import Language.Javascript.JSaddle (JSM)
import Reflex.Dom hiding (Link, mainWidgetWithCss, run)
import Reflex.Dom.Core (mainWidgetWithCss)
run2 :: JSM () -> IO ()
run2 = run 3001
appTitle :: T.Text
appTitle = "SlowNews [on jsaddle-warp]"
#else
-- Unsupported platform
#endif
