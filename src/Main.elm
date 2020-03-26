module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


type Msg
    = Click Int


type Player
    = Naughty
    | Crossy


type Square
    = Blank
    | Naught
    | Cross


type alias Combination =
    ( Square, Square, Square )


type alias Grid =
    List Square


type GameState
    = CurrentlyPlaying Player
    | Draw
    | Winner Player


type alias Model =
    { grid : Grid, player : Player, gameState : GameState }


init =
    { grid = List.repeat 9 Blank, player = Naughty, gameState = CurrentlyPlaying Naughty }


getSquare : Player -> Square
getSquare player =
    case player of
        Naughty ->
            Naught

        Crossy ->
            Cross

f : Combination -> Maybe Player
f combination = case combination of
    (Naught, Naught, Naught) -> Just Naughty
        

    (Cross, Cross, Cross) -> Just Crossy
    _ -> Nothing
        

checkForWin : List Combination -> Maybe Player
checkForWin combinations =
    case combinations of 
        (combination::tail) ->
            case f combination of 
                Just player -> Just player
                Nothing -> checkForWin tail
        -- (tail::combination::cn) -> f combination
        [] -> Nothing


gameState : Model -> GameState
gameState model =
    CurrentlyPlaying Naughty


generateCombinations : Grid -> List Combination
generateCombinations grid =
    case grid of
        [ zero, one, two, three, four, five, six, seven, eight ] ->
            [ ( zero, one, two ), ( three, four, five ), ( six, seven, eight ) ]

        _ ->
            []


update : Msg -> Model -> Model
update msg model =
    case msg of
        Click index ->
            let
                grid = List.take index model.grid ++ [ getSquare model.player ] ++ List.drop (index + 1) model.grid
                combinations = generateCombinations grid
                winner = checkForWin combinations

                player =
                    if model.player == Naughty then
                        Crossy

                    else
                        Naughty
                gs = case winner of 
                    Just p -> Winner p
                    Nothing -> CurrentlyPlaying player
            in
            
                { grid = grid
                , player = player
                , gameState = gs
                }


updateGrid : Grid -> Grid
updateGrid model =
    List.repeat 9 Cross


viewSquare : Int -> Square -> Html Msg
viewSquare index square =
    let
        childElements =
            if modBy 3 (index + 1) == 0 then
                [ br [] [] ]

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


viewPlayer : Player -> String
viewPlayer player =
    case player of
        Naughty ->
            "Naughty"

        Crossy ->
            "Crossy"


view : Model -> Html Msg
view model =
    case model.gameState of
        Winner player ->
            text (viewPlayer player ++ " has done it 🎉")

        _ ->
            div
                [ style "height" "100vh"
                , style "width" "100vw"
                , style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                ]
                [ div []
                    (List.indexedMap viewSquare model.grid)
                , h1 [] [ text (viewPlayer model.player) ]
                ]
