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
import           Data.Time                     (getCurrentTime)
import           Data.Time.Format              (defaultTimeLocale, formatTime)
import           GHC.Generics
import           JavaScript.Web.Location       (getHostname, getWindowLocation)
import           JavaScript.Web.XMLHttpRequest

import           Miso                          hiding (defaultOptions)
import           Miso.String

foreign import javascript unsafe "console.log ($1);"
  consoleLog :: MisoString -> IO ()

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
  { url      :: MisoString
  , title    :: MisoString
  , meta_url :: MisoString
  , site     :: MisoString
  , created  :: Int
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
  logInfo "Fetching from server"
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

-- | Log the given message to stdout and browser console
logInfo :: String -> IO ()
logInfo s = do
  now <- currentTime
  let msg = now <> ": " <> s
  putStrLn msg
  consoleLog $ pack msg
  where
    currentTime = do
      time <- getCurrentTime
      return $ formatTime defaultTimeLocale "%F %T (%Z)" time

-- | Main entry point
main :: IO ()
main = do
  logInfo "Starting application"
  startApp App {model = Model Nothing, initialAction = FetchLinks, ..}
  where
    update = updateModel
    events = defaultEvents
    subs   = []
    view   = viewModel


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
              [(pack "margin", pack "50px")]
        ]
        [ link_ [ rel_ $ pack "stylesheet "
                , href_ $ pack "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.1/css/materialize.min.css"] []
        , script_ [ src_ $ pack "https://code.jquery.com/jquery-3.2.1.min.js " ] []
        , script_ [ src_ $ pack "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.1/js/materialize.min.js" ] []
        , h1_ [class_ $ pack "title"] [text $ pack "SlowNews"]
        , case links of
            Nothing            -> div_ [] [text $ pack "No data"]
            Just (Links links) -> table_ [ class_ $ pack "striped" ] [ row $ viewLink <$> links ]
        ]
      where
        row links =
          tbody_ [] $ (\e -> tr_ [] [e]) <$> links

viewLink :: Link -> View Action
viewLink Link {..} = tr_ [] [timeUI, siteUI, linkUI]
  where
    timeUI = tableCell $ text $ (pack . show) created
    siteUI = tableCell $ a_ [href_ meta_url ] [ text site ]
    linkUI = tableCell $ a_ [ href_ url ] [ text title ]
    tableCell e = td_ [] [e]