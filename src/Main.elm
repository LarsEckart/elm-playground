module Main exposing (main)

import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)


type Msg
    = CreateGame
    | JoinGame


view : Html Msg
view =
    div []
        [ h1 [] [ text "Welcome to Skyjo!" ]
        , button [ onClick CreateGame ] [ text "Create Game" ]
        , button [ onClick JoinGame ] [ text "Join Game" ]
        ]


main : Html Msg
main =
    view
