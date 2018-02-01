{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module SlowNews.Native (appTitle) where

import Data.Text as T

#if defined(ghcjs_HOST_OS)
appTitle :: T.Text
appTitle = "SlowNews"
#elif defined(MIN_VERSION_jsaddle_warp)
appTitle :: T.Text
appTitle = "SlowNews [jsaddle-warp]"
#else
appTitle :: T.Text
appTitle = "SlowNews [unknown]"
#endif
