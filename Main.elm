module Main
    exposing
        ( main
        )

import Html exposing (Html, text, div, h1, h2, button)
import Html.Attributes as HA exposing (id, class)
import Html.Events exposing (onClick)
import Random exposing (Seed)
import Keyboard
import Time
import List.Extra as List


type MoveDirection
    = MoveNone
    | MoveUp
    | MoveDown


type PlayerType
    = HumanPlayer
    | AIPlayer


playerType : PlayerType
playerType =
    AIPlayer


type MoveInfo
    = Manual MoveDirection
    | Calculated


type Msg
    = PlayerMove MoveInfo
    | TogglePause Bool


type GameOverReason
    = FellInHole
    | GotCheese


type TotalStatus
    = Running
    | Lost
    | Won


type alias Game =
    { playerPosition : Int
    , moveCount : Int
    , cheesePosition : Int
    , holePosition : Int
    }


type alias Model =
    { score : Int
    , oldScore : Int
    , previousMove : MoveDirection
    , oldState : Int
    , randomFloat : ( Float, Seed )
    , nextMove : ( MoveDirection, Seed )
    , totalStatus : TotalStatus
    , currentGame : Game
    , previousGames : List ( Game, GameOverReason )
    , paused : Bool
    , qtable : QTable
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
        { model
            | currentGame = updatedCurrentGame
            , previousMove = direction
        }


processGameStatus : Model -> Model
processGameStatus model =
    if model.currentGame.playerPosition == model.currentGame.holePosition then
        { model
            | previousGames = ( model.currentGame, FellInHole ) :: model.previousGames
            , currentGame = startNewGame
            , score = model.score - 1
        }
    else if model.currentGame.playerPosition == model.currentGame.cheesePosition then
        { model
            | previousGames = ( model.currentGame, GotCheese ) :: model.previousGames
            , currentGame = startNewGame
            , score = model.score + 1
        }
    else
        model


processEndStatus : Model -> Model
processEndStatus model =
    if model.score <= -5 then
        { model | totalStatus = Lost }
    else if model.score >= 5 then
        { model | totalStatus = Won }
    else
        model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TogglePause paused ->
            ( { model | paused = paused }
            , Cmd.none
            )

        PlayerMove moveInfo ->
            let
                updatedModel =
                    (processGameStatus >> processEndStatus) <|
                        case moveInfo of
                            Manual direction ->
                                model
                                    |> processPlayerMove direction

                            Calculated ->
                                model
                                    |> processCalculatedPlayerMove
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
        newGame =
            startNewGame

        initialModel =
            { totalStatus = Running
            , score = 0
            , oldScore = 0
            , oldState = newGame.playerPosition
            , randomFloat = ( 0.0, Random.initialSeed 10234 )
            , nextMove = ( MoveNone, Random.initialSeed 23232 )
            , previousMove = MoveNone
            , currentGame = newGame
            , previousGames = []
            , paused = False
            , qtable = initialQTable
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
    if model.paused || model.totalStatus /= Running then
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
    case model.totalStatus of
        Running ->
            div []
                [ h1 [] [ text "Get The Cheese" ]
                , pauseButtonView model
                , h2 [] [ text <| "Score: " ++ toString model.score ]
                , gameBoardView model
                , h2 [] [ text <| "Previous Games" ]
                , previousGamesView model
                , text <| toString model.qtable
                ]

        Lost ->
            div []
                [ text "YOU LOST"
                , previousGamesView model
                , text <| toString model.qtable
                ]

        Won ->
            div []
                [ text "YOU WON"
                , previousGamesView model
                , text <| toString model.qtable
                ]


epsilon : Float
epsilon =
    0.5


discount : Float
discount =
    0.9


learningRate : Float
learningRate =
    0.2


type alias QTable =
    List ( Int, Float, Float )


initialQTable : QTable
initialQTable =
    [ ( 0, 0.2, 0.6 )
    , ( 1, -0.6, 0.6 )
    , ( 2, -0.4, 0.5 )
    , ( 3, -0.2, 0.3 )
    , ( 4, -0.1, 0.3 )
    , ( 5, 0, 0.2 )
    , ( 6, 0, 0.5 )
    , ( 7, 0.1, 0.6 )
    , ( 8, 0.3, 0.8 )
    , ( 9, 0.2, 1 )
    , ( 10, 1, 0.5 )
    , ( 11, 0.6, 0.2 )
    ]


processCalculatedPlayerMove : Model -> Model
processCalculatedPlayerMove model =
    let
        reward model =
            if model.oldScore < model.score then
                1
            else if model.oldScore > model.score then
                -1
            else
                0

        outcomeState model =
            model.currentGame.playerPosition

        calculateNewActionReward model ( state, up, down ) =
            case model.previousMove of
                MoveNone ->
                    ( state, up, down )

                MoveUp ->
                    ( state
                    , up
                        + learningRate
                        * ((reward model)
                            + discount
                            + (Maybe.withDefault 0 <| List.maximum [ up, down ])
                            - up
                          )
                    , down
                    )

                MoveDown ->
                    ( state
                    , up
                    , down
                        + learningRate
                        * ((reward model)
                            + discount
                            + (Maybe.withDefault 0 <| List.maximum [ up, down ])
                            - down
                          )
                    )

        updateQTable model =
            List.updateIf (\( state, _, _ ) -> state == model.currentGame.playerPosition) (calculateNewActionReward model) model.qtable

        nextFloat model =
            let
                generator =
                    Random.float 0 1
            in
                Random.step generator (Tuple.second model.randomFloat)

        randomMove model =
            let
                generator =
                    Random.bool
                        |> Random.map
                            (\choice ->
                                if choice then
                                    MoveUp
                                else
                                    MoveDown
                            )
            in
                Random.step generator (Tuple.second model.nextMove)

        findMoveFromQTable model =
            List.find (\( state, _, _ ) -> state == model.currentGame.playerPosition)
                model.qtable
                |> Maybe.map
                    (\( _, up, down ) ->
                        if up > down then
                            MoveUp
                        else
                            MoveDown
                    )
                |> Maybe.withDefault MoveNone

        nextMove model =
            if (Tuple.first model.randomFloat) > epsilon then
                randomMove model
            else
                ( findMoveFromQTable model, Tuple.second model.nextMove )

        updatedModel model =
            { model
                | oldScore = model.score
                , oldState = model.currentGame.playerPosition
                , qtable = updateQTable model
                , randomFloat = nextFloat model
                , nextMove = nextMove model
            }
    in
        model
            |> updatedModel
            |> processPlayerMove (Tuple.first model.nextMove)
