{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Main where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Map                      as M
import           Data.Maybe
import           GHC.Generics
import           JavaScript.Web.XMLHttpRequest

import           Miso                          hiding (defaultOptions)
import           Miso.String

-- | Model
data Model
  = Model
  { info :: Maybe APIInfo
  } deriving (Eq, Show)

-- | Action
data Action
  = FetchGitHub
  | SetGitHub APIInfo
  | NoOp
  deriving (Show, Eq)

-- | Main entry point
main :: IO ()
main = do
  startApp App { model = Model Nothing
               , initialAction = NoOp
               , ..
               }
    where
      update = updateModel
      events = defaultEvents
      subs   = []
      view   = viewModel

-- | Update your model
updateModel :: Action -> Model -> Effect Action Model
updateModel FetchGitHub m = m <# do
  SetGitHub <$> getGitHubAPIInfo
updateModel (SetGitHub apiInfo) m =
  noEff m { info = Just apiInfo }
updateModel NoOp m = noEff m

-- | View function, with routing
viewModel :: Model -> View Action
viewModel Model {..} = view
  where
    view = div_ [ style_ $ M.fromList [
                  (pack "text-align", pack "center")
                , (pack "margin", pack "200px")
                ]
               ] [
        h1_ [class_ $ pack "title" ] [ text $ pack "Miso XHR Example" ]
      , button_ attrs [
          text $ pack "Fetch JSON from https://api.github.com via XHR"
          ]
      , case info of
          Nothing -> div_ [] [ text $ pack "No data" ]
          Just APIInfo{..} ->
            table_ [ class_ $ pack "table is-striped" ] [
              thead_ [] [
                tr_ [] [
                  th_ [] [ text $ pack "URLs"]
                ]
              ]
            , tbody_ [] [
                tr_ [] [ td_ [] [ text current_user_url ] ]
              , tr_ [] [ td_ [] [ text emojis_url ] ]
              ]
            ]
          ]
      where
        attrs = [ onClick FetchGitHub
                , class_ $ pack "button is-large is-outlined"
                ] ++
                [ disabled_ $ pack "disabled"
                | isJust info
                ]

data APIInfo
  = APIInfo
  { current_user_url :: MisoString
  , emojis_url       :: MisoString
  } deriving (Show, Eq, Generic)

instance FromJSON APIInfo where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo '_' }

getGitHubAPIInfo :: IO APIInfo
getGitHubAPIInfo = do
  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String APIInfo of
    Left s  -> error s
    Right j -> pure j
  where
    req = Request { reqMethod = GET
                  , reqURI = pack "https://api.github.com"
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqWithCredentials = False
                  , reqData = NoData
                  }
