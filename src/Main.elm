module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode


type Page
    = LandingPage
    | CreateGamePage
    | GameInfoPage String
    | JoinGamePage
    | ErrorPage String


type Msg
    = CreateNewGame
    | RequestGameCreation
    | JoinGame
    | UpdatePlayerName String
    | UpdateGameId String
    | RequestGameJoin
    | ReceiveGameResponse (Result Http.Error GameId)
    | ReceiveGameJoinResponse (Result Http.Error GameId)


type alias Flags =
    ()


type alias GameId =
    { id : String }


type alias Model =
    { currentPage : Page, gameId : String, playerName : String }


gameIdDecoder : Decode.Decoder GameId
gameIdDecoder =
    Decode.map GameId
        (Decode.field "id" Decode.string)


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out."

        Http.NetworkError ->
            "Network error."

        Http.BadStatus statusCode ->
            "Bad status code: " ++ String.fromInt statusCode

        Http.BadBody body ->
            "Bad body: " ++ body


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateNewGame ->
            ( { model | currentPage = CreateGamePage }, Cmd.none )

        JoinGame ->
            ( { model | currentPage = JoinGamePage }, Cmd.none )

        UpdatePlayerName name ->
            ( { model | playerName = name }, Cmd.none )

        UpdateGameId id ->
            ( { model | gameId = id }, Cmd.none )

        RequestGameCreation ->
            ( model, createGameRequest )

        RequestGameJoin ->
            ( model, joinGameRequest )

        ReceiveGameResponse result ->
            case result of
                Ok gameId ->
                    ( { model | currentPage = GameInfoPage gameId.id }, Cmd.none )

                Err error ->
                    ( { model | currentPage = ErrorPage (httpErrorToString error) }, Cmd.none )

        ReceiveGameJoinResponse result ->
            case result of
                Ok gameId ->
                    ( { model | currentPage = GameInfoPage gameId.id }, Cmd.none )

                Err error ->
                    ( { model | currentPage = ErrorPage (httpErrorToString error) }, Cmd.none )


createGameRequest : Cmd Msg
createGameRequest =
    Http.post
        { url = "https://play-skyjo-ae5db0018a23.herokuapp.com/api/v1.0/skyjo/games"
        , body = Http.emptyBody
        , expect = Http.expectJson ReceiveGameResponse gameIdDecoder
        }



-- TODO: Implement the joinGameRequest with actual game ID and player name


joinGameRequest : Cmd Msg
joinGameRequest =
    Http.post
        { url = "https://play-skyjo-ae5db0018a23.herokuapp.com/api/v1.0/skyjo/games"
        , body = Http.emptyBody
        , expect = Http.expectJson ReceiveGameResponse gameIdDecoder
        }


view : Model -> Html Msg
view model =
    case model.currentPage of
        LandingPage ->
            div []
                [ h1 [] [ text "Welcome to Skyjo!" ]
                , button [ onClick CreateNewGame ] [ text "Create Game" ]
                , button [ onClick JoinGame ] [ text "Join Game" ]
                ]

        CreateGamePage ->
            div []
                [ h1 [] [ text "New game" ]
                , div [] [ text "Your name", input [ placeholder "Name" ] [] ]

                -- Add your game creation form or instructions here
                , button [ onClick RequestGameCreation ] [ text "Create" ]
                ]

        GameInfoPage id ->
            div []
                [ h1 [] [ text "Game" ]
                , div [] [ text ("Game ID: " ++ id) ]
                ]

        JoinGamePage ->
            div []
                [ h1 [] [ text "Join game" ]
                , div [] [ text "Your name", input [ placeholder "Name", onInput UpdatePlayerName ] [] ]
                , div [] [ text "Game ID", input [ placeholder "Game ID", onInput UpdateGameId ] [] ]
                , button [ onClick RequestGameJoin ] [ text "Join" ] -- Changed to RequestGameJoin
                ]

        ErrorPage message ->
            div []
                [ h1 [] [ text "Error" ]
                , div [] [ text ("An error occurred: " ++ message) ]
                ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentPage = LandingPage, playerName = "", gameId = "" }, Cmd.none )



-- Subscriptions, not used in this example but necessary for a complete Elm program


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Main function definition using Browser.sandbox for simplicity


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
