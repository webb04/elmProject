module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
main =
  Browser.sandbox { init = init, update = update, view = view }

type Msg = Click Int
type Player = Naughty | Crossy
type Square = Blank | Naught | Cross

type alias Grid = List Square

type GameState = CurrentlyPlaying Player | Draw | Winner Player
type alias Model = { grid: Grid, player: Player, gameState: GameState }

init = { grid = List.repeat 9 Blank, player = Naughty, gameState = CurrentlyPlaying Naughty }

getSquare: Player -> Square
getSquare player =
  if (player == Naughty) then Naught else Cross

checkForWin: Grid -> Player -> Maybe Player
checkForWin grid player = Nothing

gameState: GameState
gameState = Winner Naughty

update: Msg -> Model -> Model
update msg model =
  case msg of
    Click index -> 
      { 
        grid = (List.take index model.grid) ++ [getSquare model.player] ++ (List.drop (index + 1) model.grid),
        player = if (model.player == Naughty) then Crossy else Naughty,
        gameState = gameState
      }

updateGrid: Grid -> Grid
updateGrid model =
  List.repeat 9 Cross

viewSquare: Int -> Square -> Html Msg
viewSquare index square =
  let 
    childElements =
      if modBy 3 (index + 1) == 0 then
        [br [] []]
      else
        []
  in

  case square of
    Blank ->
        span [ onClick (Click index) ] ([ text "⬛️" ] ++ childElements)
    Cross ->
      span [] ([ text "❌" ] ++ childElements)
    Naught ->
      span [] ([ text "🔴" ] ++ childElements)

viewPlayer: Player -> String
viewPlayer player =
  case player of
     Naughty -> "Naughty"
     Crossy -> "Crossy"

view: Model -> Html Msg
view model =
  case model.gameState of
    Winner player ->
      text ((viewPlayer player) ++ " has done it 🎉")
    _ -> 
      div [
        style "height" "100vh",
        style "width" "100vw",
        style "display" "flex",
        style "justify-content" "center",
        style "align-items" "center"]
        [ div []
          (List.indexedMap viewSquare model.grid), h1 [] [text (viewPlayer model.player)] ]
  