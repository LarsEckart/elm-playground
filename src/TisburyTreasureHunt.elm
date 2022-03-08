module TisburyTreasureHunt exposing (..)

-- Consider defining a type alias for TreasureLocation,
-- Treasure, PlaceLocation and Place,
-- and using them in the function type annotations


type alias TreasureLocation =
    ( Int, Char )


type alias Treasure =
    ( String, TreasureLocation )


type alias PlaceLocation =
    ( Char, Int )


type alias Place =
    ( String, ( Char, Int ) )


placeLocationToTreasureLocation : PlaceLocation -> TreasureLocation
placeLocationToTreasureLocation placeLocation =
    ( Tuple.second placeLocation
    , Tuple.first placeLocation
    )


treasureLocationMatchesPlaceLocation : PlaceLocation -> TreasureLocation -> Bool
treasureLocationMatchesPlaceLocation placeLocation treasureLocation =
    placeLocationToTreasureLocation placeLocation == treasureLocation


countPlaceTreasures : Place -> List Treasure -> Int
countPlaceTreasures place treasures =
    List.length treasures


specialCaseSwapPossible : Treasure -> Place -> Treasure -> Bool
specialCaseSwapPossible ( foundTreasure, _ ) ( place, _ ) ( desiredTreasure, _ ) =
    case ( foundTreasure, place, desiredTreasure ) of
        ( "Brass Spyglass", "Abandoned Lighthouse", _ ) ->
            True

        ( "Amethyst Octopus", "Stormy Breakwater", "Crystal Crab" ) ->
            True

        ( "Amethyst Octopus", "Stormy Breakwater", "Glass Starfish" ) ->
            True

        ( "Vintage Pirate Hat", "Harbor Managers Office", "Model Ship in Large Bottle" ) ->
            True

        ( "Vintage Pirate Hat", "Harbor Managers Office", "Antique Glass Fishnet Float" ) ->
            True

        _ ->
            False
