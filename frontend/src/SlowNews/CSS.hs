{-# LANGUAGE OverloadedStrings #-}

module SlowNews.CSS (cssInline) where

import Data.ByteString (ByteString)

cssInline :: ByteString
cssInline = "\
\ @import url(https://fonts.googleapis.com/css?family=Muli); \
\ @import url(https://fonts.googleapis.com/css?family=Autour+One); \
\  \
\ html { \
\     margin:    0 auto; \
\     max-width: 960px; \
\     font-family: 'Muli', sans-serif; \
\     font-size: 85%; \
\ } \
\  \
\ h1 { \
\     font-family: 'Autour One', cursive; \
\     font-size: 1.5em; \
\     background-color: pink; \
\     padding: 0.3em; \
\ } \
\  \
\ td { \
\     padding: 0.2em; \
\     padding-bottom: 0.5em; \
\ } \
\  \
\ td.meta { \
\     text-align: right; \
\     padding-right: 0.5em; \
\ } \
\  \
\ a { \
\     text-decoration: none; \
\ } \
\ a:hover { \
\     background-color: #efefef; \
\ } \
\  \
\ .meta a { \
\     font-size: 85%; \
\     text-decoration: none; \
\     color: pink; \
\     background-color: #444; \
\     padding: 0.2em 1em; \
\ } \
\  \
\ .footer { \
\     text-align: center; \
\     padding-top: 1em; \
\     margin-top: 2em; \
\     margin-bottom: 2em; \
\     border-top: solid gray; \
\ } \
\ "
