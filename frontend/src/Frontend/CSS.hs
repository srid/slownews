{-# LANGUAGE OverloadedStrings #-}

module Frontend.CSS (appCssStr) where

import Data.String.Conv (toS)
import Data.Text (Text)

import Clay

appCssStr :: Text
appCssStr = toS $ render appCss

themeColor :: Color
themeColor = lightgreen

appCss :: Css
appCss = do
  importUrl "https://fonts.googleapis.com/css?family=Muli"
  importUrl "https://fonts.googleapis.com/css?family=Autour+One"

  html ? do
    sym2 margin (px 0) auto
    fontFamily ["Muli"] [sansSerif]

  h1 ? do
    fontFamily ["Autour One"] [cursive]
    fontSize $ em 1.5
    backgroundColor themeColor
    sym padding $ em 0.3

  td ? do
    sym padding $ em 0.2
    paddingBottom $ em 0.5

  "td.meta" ? do
    textAlign $ alignSide sideRight
    color gray

  a ? do
    textDecoration none

  "a:visited" ? do
    textDecoration lineThrough
    color gray

  "a:hover" ? do
    backgroundColor "#efefef"

  ".meta a" ? do
    fontSize $ pct 85
    textDecoration none
    color lightgreen
    backgroundColor "#444"
    sym2 padding (em 0.2) (em 1)

  ".footer" ? do
    textAlign center
    paddingTop $ em 1
    marginTop $ em 2
    marginBottom $ em 2
    borderTop solid (px 1) gray
