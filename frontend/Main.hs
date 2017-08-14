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
import           JavaScript.Web.Location       (getHostname, getWindowLocation)
import           JavaScript.Web.XMLHttpRequest

import           Miso                          hiding (defaultOptions)
import           Miso.String

-- | Model
data Model = Model
  { links :: Maybe Links
  } deriving (Eq, Show)

-- | Action
data Action
  = FetchLinks
  | SetLinks Links
  | NoOp
  deriving (Show, Eq)

-- | Link
data Link = Link
  { url   :: MisoString
  , title :: MisoString
  } deriving (Show, Eq, Generic)

-- | Links (Full API data)
newtype Links =
  Links [Link]
  deriving (Show, Eq, Generic)

instance FromJSON Link where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo '_'}

instance FromJSON Links where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo '_'}

-- FIXME: Handle errors
getLinks :: IO Links
getLinks = do
  _ <- putStrLn "Fetching.."
  url <- location
  Just resp <- contents <$> xhrByteString (req url)
  case eitherDecodeStrict resp :: Either String Links of
    Left s  -> error s
    Right j -> pure j
  where
    location = do
      hostname <- getWindowLocation >>= getHostname
      return $ "http://" <> (unpack hostname) <> ":3000/data"
    req url =
      Request
      { reqMethod = GET
      , reqURI = pack url
      , reqLogin = Nothing
      , reqHeaders = []
      , reqWithCredentials = False
      , reqData = NoData
      }

-- | Main entry point
main :: IO ()
main = do
  startApp App {model = Model Nothing, initialAction = FetchLinks, ..}
  where
    update = updateModel
    events = defaultEvents
    subs = []
    view = viewModel

-- | Update your model
updateModel :: Action -> Model -> Effect Action Model
updateModel FetchLinks m       = m <# do SetLinks <$> getLinks
updateModel (SetLinks links) m = noEff m {links = Just links}
updateModel NoOp m             = noEff m

-- | View function, with routing
viewModel :: Model -> View Action
viewModel Model {..} = view
  where
    view =
      div_
        [ style_ $
          M.fromList
            [(pack "text-align", pack "center"), (pack "margin", pack "200px")]
        ]
        [ h1_ [class_ $ pack "title"] [text $ pack "SlowNews XHR Playground"]
        , button_ attrs [text $ pack "Fetch JSON via XHR"]
        , case links of
            Nothing            -> div_ [] [text $ pack "No data"]
            Just (Links links) -> div_ [] [text $ pack $ show links]
        ]
      where
        attrs =
          [onClick FetchLinks, class_ $ pack "button is-large is-outlined"] ++
          [disabled_ $ pack "disabled" | isJust links]
