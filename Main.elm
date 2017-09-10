module Main
    exposing
        ( main
        )

import Html exposing (Html, text, div, h1, h2)
import Html.Attributes as HA exposing (id, class)
import Keyboard


type MoveDirection
    = MoveNone
    | MoveUp
    | MoveDown


type Msg
    = PlayerMove MoveDirection


type GameOverReason
    = FellInHole
    | GotCheese


type alias Game =
    { playerPosition : Int
    , moveCount : Int
    , cheesePosition : Int
    , holePosition : Int
    }


type alias Model =
    { currentGame : Game
    , previousGames : List ( Game, GameOverReason )
    }


processPlayerMove : MoveDirection -> Model -> Model
processPlayerMove direction model =
    let
        positionOffset =
            case direction of
                MoveNone ->
                    0

                MoveUp ->
                    if model.currentGame.playerPosition >= 1 then
                        -1
                    else
                        0

                MoveDown ->
                    if model.currentGame.playerPosition <= 8 then
                        1
                    else
                        0

        currentGame =
            model.currentGame

        updatedCurrentGame =
            { currentGame
                | playerPosition = currentGame.playerPosition + positionOffset
                , moveCount = currentGame.moveCount + (Basics.abs positionOffset)
            }
    in
        { model | currentGame = updatedCurrentGame }


processGameStatus : Model -> Model
processGameStatus model =
    model



-- let
-- newStatus =
-- if model.currentGame.playerPosition == model.currentGame.holePosition then
-- GameOver FellInHole
-- else if model.currentGame.playerPosition == model.currentGame.cheesePosition then
-- GameOver GotCheese
-- else
-- Running
-- in
-- { model | gameStatus = newStatus }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayerMove direction ->
            let
                updatedModel =
                    model
                        |> processPlayerMove direction
                        |> processGameStatus
            in
                ( updatedModel, Cmd.none )


startNewGame : Game
startNewGame =
    { playerPosition = 5
    , moveCount = 0
    , holePosition = 0
    , cheesePosition = 9
    }


init : ( Model, Cmd Msg )
init =
    let
        initialModel =
            { currentGame = startNewGame
            , previousGames = []
            }
    in
        ( initialModel
        , Cmd.none
        )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


calculateDirection : Keyboard.KeyCode -> MoveDirection
calculateDirection keycode =
    case keycode of
        38 ->
            MoveUp

        40 ->
            MoveDown

        _ ->
            MoveNone


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.ups (calculateDirection >> PlayerMove)


gameBoardView : Model -> Html Msg
gameBoardView model =
    let
        content index =
            if index == model.currentGame.holePosition then
                text "O"
            else if index == model.currentGame.cheesePosition then
                text "C"
            else if index == model.currentGame.playerPosition then
                text "P"
            else
                text ""

        makeDiv index =
            div [ id <| "square-" ++ toString index ] [ content index ]
    in
        div [] <|
            List.map makeDiv (List.range 0 9)


view : Model -> Html Msg
view model =
    div [ class "game-board" ]
        [ h1 [] [ text "Get The Cheese" ]
        , h2 [] [ text <| "Moves: " ++ toString model.currentGame.moveCount ]
        , gameBoardView model
        ]
