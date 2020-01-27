module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = init, update = update, view = view }

type Msg = Click

type Square = Blank | Naught | Cross

type alias Grid = List (List Square)
type alias Model = Grid

init = List.repeat 3 (List.repeat 3 Blank)

viewRow: List Square -> Html Msg
viewRow squares =
  div []
    (List.map viewSquare squares)

update: Msg -> Model -> Model
update msg model =
  case msg of
    Click -> 
      updateGrid model

updateGrid: Grid -> Grid
updateGrid model =
  List.repeat 3 (List.repeat 3 Cross)

viewSquare: Square -> Html Msg
viewSquare square =
  case square of
    Blank ->
      span [ onClick Click ] [ text "⬛️" ]
    Cross ->
      span [] [ text "❌" ]
    Naught ->
      span [] [ text "🔴" ]

view: Model -> Html Msg
view model =
  div []
    (List.map viewRow model)