{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import qualified Data.Map    as M

import           Miso
import           Miso.String (MisoString, pack)
import           Miso.Svg    hiding (height_, id_, style_, width_)

main :: IO ()
main = startApp App {..}
  where
    initialAction = Id
    model         = emptyModel
    update        = updateModel
    view          = viewModel
    events        = defaultEvents
    subs          = [ mouseSub HandleMouse ]

emptyModel :: Model
emptyModel = Model (0,0)

updateModel :: Action -> Model -> Effect Action Model
updateModel (HandleMouse newCoords) model =
  noEff model { mouseCoords = newCoords }
updateModel Id model = noEff model

data Action
  = HandleMouse (Int, Int)
  | Id

newtype Model
  = Model
  { mouseCoords  :: (Int, Int)
  } deriving (Show, Eq)

viewModel :: Model -> View Action
viewModel (Model (x,y)) =
  div_ [ ] [
    svg_ [ width_ "100%"
         , style_ $ M.fromList [ ("border-style", "solid")
                               , ("height", "700px")
                               ]
       ] [
     ellipse_ [ cx_ $ pack $ show x
              , cy_ $ pack $ show y
              , style_ svgStyle
              , rx_ "100"
              , ry_ "100"
              ] []
   ]
 ]

svgStyle :: M.Map MisoString MisoString
svgStyle =
  M.fromList [
      ("fill", "blue")
    , ("stroke", "black")
    , ("stroke-width", "2")
    ]
