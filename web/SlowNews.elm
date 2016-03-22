import Debug exposing (log)
import Http
import Html.Attributes exposing (..)
import Signal
import Char
import String
import Result
import List
import Maybe
import Result
import Date
import Set
import Task exposing (..)
import Html as H
import Html exposing (Html)
import Json.Decode  as J
import Json.Decode exposing ((:=))
import Json.Encode as JE

-- Model

type alias Link =
  { title     : String
  , url       : String
  , metaUrl   : String
  , created   : Date.Date
  , site      : String
  }

type alias Model =
  List Link

type alias Data =
  Result Http.Error Model

summarizeLinks : List Link -> String
summarizeLinks =
  (List.map .site) >> Set.fromList >> Set.toList >> List.sort >> String.join ":"

-- JSON decoders

andMap : J.Decoder (a -> b) -> J.Decoder a -> J.Decoder b
andMap = J.object2 (<|)

dateFromUnix unixtime =
  unixtime * 1000 |> toFloat |> Date.fromTime |> Result.Ok

decodeDate : J.Decoder (Date.Date)
decodeDate = J.customDecoder J.int dateFromUnix

decodeLink : J.Decoder Link
decodeLink = Link
  `J.map`   ("title"     := J.string)
  `andMap`  ("url"       := J.string)
  `andMap`  ("meta_url"  := J.string)
  `andMap`  ("created"   := decodeDate)
  `andMap`  ("site"      := J.string)

decodeModel : J.Decoder Model
decodeModel = J.list decodeLink


-- Main routines

getData : Task never Data
getData =
  Http.get decodeModel "/data" |> Task.toResult

dataMailbox : Signal.Mailbox Data
dataMailbox =
  Signal.mailbox <| Ok []

port runner : Task never ()
port runner =
  getData `andThen` (Signal.send dataMailbox.address)

main : Signal Html
main =
  Signal.map view dataMailbox.signal


-- View

view : Data -> Html
view data =
  case data of
    Err error -> H.div [] [H.text <| "API Error: " ++ (toString error)]
    Ok links -> viewData links

viewData links =
  let
    mainView = viewLinks links
    allViews  = [mainView, viewFooter]
  in
    H.div [] allViews

viewLinks : List Link -> Html
viewLinks links =
  let
    orderedLinks  = links |> List.sortBy (.created >> Date.toTime) |> List.reverse
  in
    H.div [class "site"]
       [ H.h2 [] [H.text "SlowNews"]
       , H.ul [] <| List.map viewLink orderedLinks ]

viewLink : Link -> Html
viewLink link =
  H.tr []
     [ H.td [] [H.text <| (link.created |> Date.dayOfWeek |> toString)]
     , H.td [class "meta"] [viewMeta link.site link.metaUrl ]
     , H.td [] [H.a [href link.url] [H.text link.title]]
     ]

viewMeta : String -> String -> Html
viewMeta name url =
  let
    shortName = shortenSiteName name
    cssStyle = style [
                ("class", "meta")
               ,("color", "pink")
               ,("background-color", "#444")
               ]
  in
    H.a [cssStyle, href url] [H.text shortName]

viewFooter : Html
viewFooter =
  H.div [id "footer"]
   [ H.a [href "https://github.com/srid/slownews"] [H.text "Fork SlowNews on GitHub"]]

-- View util

shortenSiteName : String -> String
shortenSiteName =
  String.split "#" >> List.head >> Maybe.withDefault "unknown"
