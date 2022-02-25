module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { counter : Int
    , phoneNumber : String
    }


init : Model
init =
    { counter = 0
    , phoneNumber = ""
    }



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Reset
    | PlusTen
    | NewNumber String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | counter = model.counter + 1 }

        Decrement ->
            { model | counter = model.counter - 1 }

        Reset ->
            { model | counter = 0 }

        PlusTen ->
            { model | counter = model.counter + 10 }

        NewNumber pn ->
            { model | phoneNumber = pn }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.counter) ]
        , button [ onClick Increment ] [ text "+" ]
        , button [ onClick Reset ] [ text "Reset" ]
        , button [ onClick PlusTen ] [ text "+10" ]
        , input [ placeholder "phone number here", value model.phoneNumber, onInput NewNumber ] []
        , div [] [ text (String.filter Char.isDigit model.phoneNumber) ]
        ]
