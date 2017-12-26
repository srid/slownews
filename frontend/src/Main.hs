{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import           Data.Aeson                    (FromJSON (parseJSON),
                                                defaultOptions,
                                                eitherDecodeStrict,
                                                genericParseJSON)
import           Data.Aeson.Types              (Options (fieldLabelModifier),
                                                camelTo2)
import           Data.Function                 (on)
import           Data.List                     (sortBy)
import           Data.Time                     (getCurrentTime)
import           Data.Time.Clock.POSIX         (posixSecondsToUTCTime)
import           Data.Time.Format              (defaultTimeLocale, formatTime)
import           Foreign                       (consoleLog)
import           GHC.Generics                  (Generic)
import           JavaScript.Web.XMLHttpRequest (Method (GET), Request (Request, reqData, reqHeaders, reqLogin, reqMethod, reqURI, reqWithCredentials),
                                                RequestData (NoData),
                                                Response (contents),
                                                xhrByteString)
import           Miso                          (App (App, events, initialAction, model, subs, update, view),
                                                Effect, View, a_, class_,
                                                defaultEvents, div_, h1_, href_,
                                                mountPoint, noEff, startApp,
                                                table_, tbody_, td_, text, tr_,
                                                (<#))
import           Miso.String                   (MisoString, pack, (<>))

-- | Model
newtype Model = Model
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
  { url     :: MisoString
  , title   :: MisoString
  , metaUrl :: MisoString
  , site    :: MisoString
  , created :: Int
  } deriving (Show, Eq, Generic)

-- | Links (Full API data)
newtype Links =
  Links [Link]
  deriving (Show, Eq, Generic)

instance FromJSON Link where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance FromJSON Links where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

-- FIXME: Handle errors
getLinks :: IO Links
getLinks = do
  logInfo "Fetching from server"
  Just resp <- contents <$> xhrByteString (req "/data")
  case eitherDecodeStrict resp :: Either String Links of
    Left s  -> error s
    Right j -> pure j
  where
    req url =
      Request
      { reqMethod = GET
      , reqURI = url
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
    currentTime = formatTime defaultTimeLocale "%F %T (%Z)" <$> getCurrentTime

-- | Main entry point
main :: IO ()
main = do
  logInfo "Starting application (Miso v0.10.0.0)"
  startApp App {model = Model Nothing, initialAction = FetchLinks, ..}
  startApp App {..}
  where
    initialAction = FetchLinks
    model = Model Nothing
    update = updateModel
    view = viewModel
    events = defaultEvents
    mountPoint = Nothing
    subs = []

-- | Update your model
updateModel :: Action -> Model -> Effect Action Model
updateModel FetchLinks m       = m <# do SetLinks <$> getLinks
updateModel (SetLinks links) m = noEff m {links = Just links}
updateModel NoOp m             = noEff m

-- | View function, with routing
viewModel :: Model -> View Action
viewModel Model {..} = div_ [] [title, content, footer]
  where
    title = h1_ [class_ "title"] [text "SlowNews"]
    content = viewLinks links
    footer =
      div_
        [class_ "footer"]
        [ a_
            [href_ "https://github.com/srid/slownews"]
            [text "SlowNews on GitHub"]
        ]

viewLinks :: Maybe Links -> View Action
viewLinks Nothing = div_ [] [text "No data" ]
viewLinks (Just (Links links)) = table_ [] [tbody_ [] body ]
  where
    body = viewLink <$> sortLinks links

viewLink :: Link -> View Action
viewLink Link {..} = tr_ [] [timeUI, siteUI, linkUI]
  where
    timeUI = td_ [] [text $ (pack . getDayOfWeek) created]
    siteUI = td_ [class_ "meta"] [a_ [href_ metaUrl ] [ text site ]]
    linkUI = td_ [] [a_ [ href_ url ] [ text title ]]

getDayOfWeek :: Int -> String
getDayOfWeek = dayOfWeek . posixSecondsToUTCTime . fromIntegral
  where
    dayOfWeek = formatTime defaultTimeLocale "%a"

sortLinks :: [Link] -> [Link]
sortLinks = sortBy (flip compare `on` created)
