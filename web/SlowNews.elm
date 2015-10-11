import Http
import Html.Attributes exposing (..)
import Signal
import String
import Result
import Date
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
  }

type alias Site =
  { name   : String
  , links  : List Link
  }

type alias Model =
  List Site

dateFromUnix unixtime =
  unixtime * 1000 |> toFloat |> Date.fromTime |> Result.Ok

dateToString : Date.Date -> String
dateToString date =
  String.join ""
          [ Date.dayOfWeek date |> toString
          , ", "
          , Date.month date |> toString
          , " "
          , Date.day date |> toString
          , " " ]


-- JSON decoders

andMap : J.Decoder (a -> b) -> J.Decoder a -> J.Decoder b
andMap = J.object2 (<|)

decodeDate : J.Decoder (Date.Date)
decodeDate = J.customDecoder J.int dateFromUnix 

decodeLink : J.Decoder Link
decodeLink = Link
  `J.map`   ("title"     := J.string)
  `andMap`  ("url"       := J.string)
  `andMap`  ("meta_url"  := J.string)
  `andMap`  ("created"   := decodeDate)

decodeSite : J.Decoder Site
decodeSite = Site
  `J.map`  ("name"    := J.string)
  `andMap` ("links"   := J.list decodeLink)

decodeModel : J.Decoder Model
decodeModel = J.list decodeSite

getData : Task Http.Error Model
getData =
  Http.get decodeModel "/data"

dataMailbox : Signal.Mailbox Model
dataMailbox =
  Signal.mailbox [{ name = "Loading...", links = []}]

port runner : Task Http.Error ()
port runner =
  getData `andThen` (Signal.send dataMailbox.address)

main : Signal Html
main =
  Signal.map view dataMailbox.signal

view : Model -> Html
view sites =
  let
    siteViews = List.map viewSite sites
    allViews  = siteViews ++ [viewFooter]
  in
    H.div [] allViews

viewSite : Site -> Html
viewSite site =
  let
    links = site.links |> List.sortBy (.created >> Date.toTime) |> List.reverse
  in
  H.div [class "site"]
    [ H.h2 [] [H.text site.name]
    , H.ul [] <| List.map viewLink links ]

viewLink : Link -> Html
viewLink link =
  H.li []
     [ H.a [href link.url] [H.text link.title]
     , H.text " "
     , H.a [class "meta", href link.metaUrl, title (link.created |> dateToString)] [H.text "meta"] ]

viewFooter : Html
viewFooter =
  H.div [id "footer"]
   [ H.a [href "https://github.com/srid/slownews"] [H.text "Fork SlowNews on GitHub"]]
