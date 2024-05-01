module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)


type Page
    = LandingPage
    | CreateGamePage


type Command
    = CreateNewGame
    | CreateGameCommand


type alias Model =
    { currentPage : Page }


update : Command -> Model -> Model
update command model =
    case command of
        CreateNewGame ->
            { model | currentPage = CreateGamePage }

        CreateGameCommand ->
            { model | currentPage = LandingPage }


view : Model -> Html Command
view model =
    case model.currentPage of
        LandingPage ->
            div []
                [ h1 [] [ text "Welcome to Skyjo!" ]
                , button [ onClick CreateNewGame ] [ text "Create Game" ]
                ]

        CreateGamePage ->
            div []
                [ h1 [] [ text "Create your game" ]

                -- Add your game creation form or instructions here
                , button [ onClick CreateGameCommand ] [ text "Create" ]
                ]


init : Model
init =
    { currentPage = LandingPage }



-- Subscriptions, not used in this example but necessary for a complete Elm program


subscriptions : Model -> Sub Command
subscriptions _ =
    Sub.none



-- Main function definition using Browser.sandbox for simplicity


main : Program () Model Command
main =
    Browser.sandbox { init = init, update = update, view = view }
