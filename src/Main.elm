module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random
import Time

main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type Msg
    = Click Int
    | PlayerGenerated Player
    | Tick Time.Posix


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
    { grid : Grid, player : Player, gameState : GameState, time: Int }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { grid = List.repeat 9 Blank, player = Naughty, gameState = CurrentlyPlaying Naughty, time = 10 }, Random.generate PlayerGenerated getRandomPlayer )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameState of
        CurrentlyPlaying _ ->
            Time.every 1000 Tick

        _ -> Sub.none

getRandomPlayer : Random.Generator Player
getRandomPlayer =
    Random.uniform Naughty [ Crossy ]


getSquare : Player -> Square
getSquare player =
    case player of
        Naughty ->
            Naught

        Crossy ->
            Cross


f : Combination -> Maybe Player
f combination =
    case combination of
        ( Naught, Naught, Naught ) ->
            Just Naughty

        ( Cross, Cross, Cross ) ->
            Just Crossy

        _ ->
            Nothing


checkForWin : List Combination -> Maybe Player
checkForWin combinations =
    case combinations of
        combination :: tail ->
            case f combination of
                Just player ->
                    Just player

                Nothing ->
                    checkForWin tail

        -- (tail::combination::cn) -> f combination
        [] ->
            Nothing


gameState : Model -> GameState
gameState model =
    CurrentlyPlaying Naughty


winningCombinations : Grid -> List Combination
winningCombinations grid =
    case grid of
        [ zero, one, two, three, four, five, six, seven, eight ] ->
            [ --horizontal
              ( zero, one, two )
            , ( three, four, five )
            , ( six, seven, eight )

            --vertical
            , ( zero, three, six )
            , ( one, four, seven )
            , ( two, five, eight )

            --diagonal
            , ( zero, four, eight )
            , ( two, four, six )
            ]

        _ ->
            []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click index ->
            let
                grid =
                    List.take index model.grid ++ [ getSquare model.player ] ++ List.drop (index + 1) model.grid

                combinations =
                    winningCombinations grid

                winner =
                    checkForWin combinations

                player =
                    if model.player == Naughty then
                        Crossy

                    else
                        Naughty

                gs =
                    case winner of
                        Just p ->
                            Winner p

                        Nothing ->
                            CurrentlyPlaying player
            in
            ( { model
              | grid = grid
              , player = player
              , gameState = gs
              , time = 10
              }
            , Cmd.none
            )

        PlayerGenerated player ->
            ( { model | player = player }
            , Cmd.none
            )

        Tick _ ->
            let
                newTime = model.time - 1

                gs =
                    if newTime == 0 then
                        if model.player == Naughty then
                            Winner Crossy
                        else
                            Winner Naughty
                    else
                        CurrentlyPlaying model.player

            in
            ( { model | time = newTime, gameState = gs }
            , Cmd.none
            )


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
            div
                [ style "height" "100vh"
                , style "width" "100vw"
                , style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                , style "font-family" "-apple-system, BlinkMacSystemFont"
                ]
                [
                text (viewPlayer player ++ " has done it 🎉")
                ]

        _ ->
            let
                fontSize = 
                    if model.time <= 3 then
                        String.fromInt ((4 - model.time) * 2) ++ "rem"
                    else
                        "2rem"
            in
            div
                [ style "height" "100vh"
                , style "width" "100vw"
                , style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                , style "font-family" "-apple-system, BlinkMacSystemFont"
                ]
                [ div []
                    (List.indexedMap viewSquare model.grid)
                , h1
                    [ style "position" "absolute"
                    , style "bottom" "20px"
                    , style "left" "20px"
                    , style "margin" "0"
                    , style "font-weight" "400"
                    ] [ text (viewPlayer model.player) ]
                , time
                    [ style "position" "absolute"
                    , style "bottom" "20px"
                    , style "right" "20px"
                    , style "transition" "font-size 200ms"
                    , style "font-size"  fontSize
                    , style "color" (if model.time <= 3 then "crimson" else "black")
                ] [ text (String.fromInt model.time) ]
                ]
