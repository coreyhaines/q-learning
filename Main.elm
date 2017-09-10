module Main
    exposing
        ( main
        )

import Html exposing (Html, text, div, h1, h2, button)
import Html.Attributes as HA exposing (id, class)
import Html.Events exposing (onClick)
import Keyboard
import Time


type MoveDirection
    = MoveNone
    | MoveUp
    | MoveDown


type PlayerType
    = HumanPlayer
    | AIPlayer


playerType : PlayerType
playerType =
    HumanPlayer


type MoveInfo
    = Manual MoveDirection
    | Calculated


type Msg
    = PlayerMove MoveInfo
    | TogglePause Bool


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
    , paused : Bool
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
    if model.currentGame.playerPosition == model.currentGame.holePosition then
        { model
            | previousGames = ( model.currentGame, FellInHole ) :: model.previousGames
            , currentGame = startNewGame
        }
    else if model.currentGame.playerPosition == model.currentGame.cheesePosition then
        { model
            | previousGames = ( model.currentGame, GotCheese ) :: model.previousGames
            , currentGame = startNewGame
        }
    else
        model


update msg model =
    case msg of
        TogglePause paused ->
            ( { model | paused = paused }
            , Cmd.none
            )

        PlayerMove moveInfo ->
            case moveInfo of
                Manual direction ->
                    let
                        updatedModel =
                            model
                                |> processPlayerMove direction
                                |> processGameStatus
                    in
                        ( updatedModel, Cmd.none )

                Calculated ->
                    let
                        updatedModel =
                            model
                                |> processPlayerMove MoveUp
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
            , paused = False
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


playerSubscription : Model -> Sub MoveInfo
playerSubscription model =
    case playerType of
        HumanPlayer ->
            Keyboard.ups (calculateDirection >> Manual)

        AIPlayer ->
            Time.every (1 * Time.second) (\_ -> Calculated)


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused then
        Sub.none
    else
        playerSubscription model
            |> Sub.map PlayerMove


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
        div [ class "game-board" ] <|
            List.map makeDiv (List.range 0 9)


previousGameView : ( Game, GameOverReason ) -> Html Msg
previousGameView ( game, gameOverReason ) =
    let
        description =
            toString gameOverReason ++ " in " ++ toString game.moveCount ++ " move(s)."
    in
        div []
            [ text description ]


previousGamesView : Model -> Html Msg
previousGamesView model =
    div [] <|
        List.map previousGameView model.previousGames


pauseButtonView : Model -> Html Msg
pauseButtonView model =
    if model.paused then
        button [ onClick (TogglePause False) ] [ text "Unpause" ]
    else
        button [ onClick (TogglePause True) ] [ text "Pause" ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Get The Cheese" ]
        , pauseButtonView model
        , h2 [] [ text <| "Moves: " ++ toString model.currentGame.moveCount ]
        , gameBoardView model
        , h2 [] [ text <| "Previous Games" ]
        , previousGamesView model
        ]
