module BettysBikeShop exposing (penceToPounds, poundsToString)

import String



-- TODO: define the type annotations for the functions
-- TODO: define body of functions


penceToPounds : Int -> Float
penceToPounds pence =
    toFloat pence / 100.0


poundsToString : Float -> String
poundsToString pounds =
    "£" ++ String.fromFloat pounds
