module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)



-- MAIN


main =
    view initialModel



-- MODEL


initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }



-- UPDATE
-- VIEW


view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ]
            (List.map viewThumbnail model.photos)
        ]


urlPrefix =
    "http://elm-in-action.com/"


viewThumbnail thumb =
    img [ src (urlPrefix ++ thumb.url) ] []
