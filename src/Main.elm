module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)


type AppState
    = LandingPage
    | CreateGamePage


type Msg
    = CreateGame
    | CreateGameCommand


type alias Model =
    { currentState : AppState }


update : Msg -> Model -> Model
update msg model =
    case msg of
        CreateGame ->
            { model | currentState = CreateGamePage }

        CreateGameCommand ->
            { model | currentState = LandingPage }


view : Model -> Html Msg
view model =
    case model.currentState of
        LandingPage ->
            div []
                [ h1 [] [ text "Welcome to Skyjo!" ]
                , button [ onClick CreateGame ] [ text "Create Game" ]
                ]

        CreateGamePage ->
            div []
                [ h1 [] [ text "Create your game" ]

                -- Add your game creation form or instructions here
                , button [ onClick CreateGameCommand ] [ text "Create" ]
                ]


init : Model
init =
    { currentState = LandingPage }



-- Subscriptions, not used in this example but necessary for a complete Elm program


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Main function definition using Browser.sandbox for simplicity


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
