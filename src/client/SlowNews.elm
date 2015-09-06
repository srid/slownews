import Http
import Signal
import Task exposing (..)
import Html as H
import Html exposing (Html)

getData : Task Http.Error String
getData =
  Http.getString "/data"

dataMailbox : Signal.Mailbox String
dataMailbox =
  Signal.mailbox "Initializing.. "

port runner : Task Http.Error ()
port runner =
  getData `andThen` (Signal.send dataMailbox.address)

main : Signal Html
main =
  Signal.map view dataMailbox.signal

view : String -> Html
view data =
  H.code [] [H.text data]
