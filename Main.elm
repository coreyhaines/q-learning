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


type GameStatus
    = Running
    | GameOver GameOverReason


type alias Model =
    { playerPosition : Int
    , moveCount : Int
    , cheesePosition : Int
    , holePosition : Int
    , gameStatus : GameStatus
    }


processPlayerMove : MoveDirection -> Model -> Model
processPlayerMove direction model =
    let
        positionOffset =
            case direction of
                MoveNone ->
                    0

                MoveUp ->
                    if model.playerPosition >= 1 then
                        -1
                    else
                        0

                MoveDown ->
                    if model.playerPosition <= 8 then
                        1
                    else
                        0
    in
        { model | playerPosition = model.playerPosition + positionOffset }


processGameStatus : Model -> Model
processGameStatus model =
    let
        newStatus =
            if model.playerPosition == model.holePosition then
                GameOver FellInHole
            else if model.playerPosition == model.cheesePosition then
                GameOver GotCheese
            else
                Running
    in
        { model | gameStatus = newStatus }


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


init : ( Model, Cmd Msg )
init =
    let
        initialModel =
            { playerPosition = 5
            , moveCount = 0
            , holePosition = 0
            , cheesePosition = 9
            , gameStatus = Running
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
    if model.gameStatus == Running then
        Keyboard.ups (calculateDirection >> PlayerMove)
    else
        Sub.none


gameBoardView : Model -> Html Msg
gameBoardView model =
    let
        content index =
            if index == model.holePosition then
                text "O"
            else if index == model.cheesePosition then
                text "C"
            else if index == model.playerPosition then
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
        , h2 [] [ text <| "Status: " ++ toString model.gameStatus ]
        , gameBoardView model
        ]
