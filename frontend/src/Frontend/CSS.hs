{-# LANGUAGE OverloadedStrings #-}

module Frontend.CSS (cssInline) where

import Data.Text (Text)

-- TODO: Switch to clay
cssInline :: Text
cssInline = "\
\ @import url(https://fonts.googleapis.com/css?family=Muli); \
\ @import url(https://fonts.googleapis.com/css?family=Autour+One); \
\ @import url(https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.1/semantic.min.css); \
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
\     background-color: lightgreen; \
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
\ a:visited { \
\     text-decoration: line-through; \
\     color: gray; \
\ } \
\ a:hover { \
\     background-color: #efefef; \
\ } \
\  \
\ .meta a { \
\     font-size: 85%; \
\     text-decoration: none; \
\     color: lightgreen; \
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
