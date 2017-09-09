module Main
    exposing
        ( main
        )

import Html exposing (Html, text, div, h1)
import Html.Attributes as HA exposing (id, class)
import Keyboard


type MoveDirection
    = MoveNone
    | MoveUp
    | MoveDown


type Msg
    = PlayerMove MoveDirection


type alias Player =
    { position : Int
    }


type alias Model =
    { player : Player
    }


playerIsAtIndex : Int -> Player -> Bool
playerIsAtIndex index { position } =
    index == position


gameBoardView : Model -> Html Msg
gameBoardView model =
    let
        content index =
            case index of
                0 ->
                    text "O"

                9 ->
                    text "C"

                _ ->
                    if playerIsAtIndex index model.player then
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
        , gameBoardView model
        ]


processPlayerMove : MoveDirection -> Model -> ( Model, Cmd Msg )
processPlayerMove direction model =
    let
        positionOffset =
            case direction of
                MoveNone ->
                    0

                MoveUp ->
                    if player.position >= 1 then
                        -1
                    else
                        0

                MoveDown ->
                    if player.position <= 8 then
                        1
                    else
                        0

        player =
            model.player

        updatedPlayer =
            { player | position = player.position + positionOffset }
    in
        ( { model | player = updatedPlayer }
        , Cmd.none
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayerMove direction ->
            processPlayerMove direction model


initialPlayer : Player
initialPlayer =
    { position = 5 }


init : ( Model, Cmd Msg )
init =
    let
        initialModel =
            { player = initialPlayer
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
