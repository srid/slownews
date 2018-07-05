{-# LANGUAGE OverloadedStrings #-}

module Frontend.CSS (cssInline, appCssStr) where

import Data.Text (Text)
import Data.String.Conv (toS)

import Clay

appCssStr :: Text
appCssStr = toS $ render appCss

themeColor :: Color
themeColor = lightgreen

appCss :: Css
appCss = do
  html ? do
    sym2 margin (px 0) auto
    fontFamily ["Muli"] [sansSerif]

  h1 ? do
    fontFamily ["Autour One"] [cursive]
    fontSize $ em 1.5
    backgroundColor themeColor
    sym padding $ em 0.3

-- TODO: Include semantic.min.css in repo

-- TODO: Switch to clay
cssInline :: Text
cssInline = "\
\ @import url(https://fonts.googleapis.com/css?family=Muli); \
\ @import url(https://fonts.googleapis.com/css?family=Autour+One); \
\ @import url(https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.1/semantic.min.css); \
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
