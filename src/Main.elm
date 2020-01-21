module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = 0, update = update, view = view }

type Msg = Increment | Decrement

type Square = Blank | Naught | Cross

grid = List.repeat 3 Blank
-- (List.repeat 3 Blank)

update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

viewSquare: Square -> Html Msg
viewSquare square =
  case square of
    Blank ->
      div [] [ text "⬛️" ]
    Cross ->
      div [] [ text "❌" ]
    Naught ->
      div [] [ text "🔴" ]

view model =
  div []
    (List.map viewSquare grid)