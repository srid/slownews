import Http
import Html.Attributes exposing (..)
import Signal
import Task exposing (..)
import Html as H
import Html exposing (Html)
import Json.Decode  as J
import Json.Decode exposing ((:=))
import Json.Encode as JE

type alias Link =
  { title     : String
  , url       : String
  , metaUrl   : String
  , created   : Int
  }

type alias Site =
  { name   : String
  , links  : List Link
  }

type alias Model =
  List Site

andMap : J.Decoder (a -> b) -> J.Decoder a -> J.Decoder b
andMap = J.object2 (<|)

decodeLink : J.Decoder Link
decodeLink = Link
  `J.map`   ("title"     := J.string)
  `andMap`  ("url"       := J.string)
  `andMap`  ("meta_url"  := J.string)
  `andMap`  ("created"   := J.int)

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
  H.div [] <| List.map viewSite sites

viewSite : Site -> Html
viewSite site =
  let
    links = site.links |> List.sortBy .created |> List.reverse
  in
  H.div [class "site"]
    [ H.h2 [] [H.text site.name]
    , H.ul [] <| List.map viewLink links ]

viewLink : Link -> Html
viewLink link =
  H.li []
     [ H.a [href link.url] [H.text link.title]
     , H.text " "
     , H.a [class "meta", href link.metaUrl] [H.text "meta"] ]
