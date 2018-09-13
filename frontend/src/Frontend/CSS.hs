{-# LANGUAGE OverloadedStrings #-}

module Frontend.CSS (appCssStr) where

import Prelude hiding (rem)

import Data.Semigroup ((<>))
import Data.String.Conv (toS)
import Data.Text (Text)

import Clay

appCssStr :: Text
appCssStr = toS $ render appCss

themeColor :: Color
themeColor = lightgreen

themeLinkColor :: Color
themeLinkColor = pink

mainFont :: (Text, Text)
mainFont = ("Comfortaa:700", "Comfortaa")

headerFont :: (Text, Text)
headerFont = ("Faster+One", "Faster One")

appCss :: Css
appCss = do
  importUrl $ "https://fonts.googleapis.com/css?family=" <> fst mainFont
  importUrl $ "https://fonts.googleapis.com/css?family=" <> fst headerFont

  ".content" ? do
    important $ fontFamily [snd mainFont] [sansSerif]

  h1 ? do
    textAlign center
    important $ fontFamily [snd headerFont] [cursive]
    fontSize $ em 1.5
    important $ color themeColor
    sym padding $ em 0.3

  ".ui.table.links" ? do
    a ? do
      textDecoration none
      color themeLinkColor

    "a:visited" ? do
      textDecoration lineThrough
      color gray

    ".meta a" ? do
      fontSize $ pct 85
      textDecoration none
      color themeColor
      backgroundColor "#444"
      sym2 padding (em 0.2) (em 0.4)
