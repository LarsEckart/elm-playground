module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ]
            (List.map viewThumbnail [ "1.jpeg", "2.jpeg", "3.jpeg" ])
        ]


urlPrefix =
    "http://elm-in-action.com/"


viewThumbnail thumb =
    img [ src (urlPrefix ++ thumb) ] []


main =
    view "no model yet"
