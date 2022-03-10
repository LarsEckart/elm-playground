module Tests exposing (tests)

import AnnalynsInfiltration exposing (canFastAttack, canFreePrisoner, canSignalPrisoner, canSpy, stealthAttackDamage)
import Bandwagoner exposing (..)
import BettysBikeShop exposing (..)
import Expect exposing (FloatingPointTolerance(..))
import LuigisLusciousLasagna exposing (remainingTimeInMinutes)
import RolePlayingGame exposing (Player, castSpell, introduce, revive)
import Test exposing (..)
import TicketPlease exposing (..)
import TicketPleaseSupport exposing (Status(..), Ticket(..), User(..))
import TisburyTreasureHunt exposing (..)
import TracksOnTracksOnTracks exposing (..)
import ValentinesDay exposing (..)


tests : Test
tests =
    describe "All Tests"
        [ describe "BettysBikeShop"
            [ test "599 pence should be 5.99 pounds" <|
                \_ ->
                    penceToPounds 599
                        |> Expect.within (Absolute 0.001) 5.99
            , test "33 pence should be 0.33 pounds" <|
                \_ ->
                    penceToPounds 33
                        |> Expect.within (Absolute 0.001) 0.33
            , test "5.99 pounds should be formatted as £5.99" <|
                \_ ->
                    poundsToString 5.99
                        |> Expect.equal "£5.99"
            , test "0.33 pounds should be formatted as £0.33" <|
                \_ ->
                    poundsToString 0.33
                        |> Expect.equal "£0.33"

            -- Tracks on Tracks on Tracks
            , test "newList should return an empty list" <|
                \_ ->
                    newList
                        |> Expect.equal []
            , test "existingList should return Elm, Closure and Haskell" <|
                \_ ->
                    existingList
                        |> Expect.equal [ "Elm", "Clojure", "Haskell" ]
            , test "addLanguage adds language to empty list" <|
                \_ ->
                    newList
                        |> addLanguage "Scala"
                        |> Expect.equal [ "Scala" ]
            , test "addLanguage adds language to existing list" <|
                \_ ->
                    existingList
                        |> addLanguage "Common Lisp"
                        |> Expect.equal [ "Common Lisp", "Elm", "Clojure", "Haskell" ]
            , test "addLanguage adds language to custom list" <|
                \_ ->
                    addLanguage "Racket" [ "Scheme" ]
                        |> Expect.equal [ "Racket", "Scheme" ]
            , test "Count languages on new list" <|
                \_ ->
                    newList
                        |> countLanguages
                        |> Expect.equal 0
            , test "Count languages on existing list" <|
                \_ ->
                    existingList
                        |> countLanguages
                        |> Expect.equal 3
            , test "Count languages on custom list" <|
                \_ ->
                    countLanguages [ "Python", "JavaScript" ]
                        |> Expect.equal 2
            , test "Reverse order of new list" <|
                \_ ->
                    newList
                        |> reverseList
                        |> Expect.equal []
            , test "Reverse order of existing list" <|
                \_ ->
                    existingList
                        |> reverseList
                        |> Expect.equal (List.reverse existingList)
            , test "Reverse order of custom list" <|
                \_ ->
                    reverseList [ "Kotlin", "Java", "Scala", "Clojure" ]
                        |> Expect.equal [ "Clojure", "Scala", "Java", "Kotlin" ]
            , test "Empty list is not exciting" <|
                \_ ->
                    excitingList []
                        |> Expect.equal False
            , test "Singleton list with Elm is exciting" <|
                \_ ->
                    excitingList [ "Elm" ]
                        |> Expect.equal True
            , test "Singleton list without Elm is not exciting" <|
                \_ ->
                    excitingList [ "C#" ]
                        |> Expect.equal False
            , test "Two-item list with Elm as first item is exciting" <|
                \_ ->
                    excitingList [ "Elm", "Clojure" ]
                        |> Expect.equal True
            , test "Two-item list with Elm as second item is exciting" <|
                \_ ->
                    excitingList [ "Nim", "Elm" ]
                        |> Expect.equal True
            , test "Two-item list without Elm is not exciting" <|
                \_ ->
                    excitingList [ "Python", "Go" ]
                        |> Expect.equal False
            , test "Three-item list with Elm as first item is exciting" <|
                \_ ->
                    excitingList [ "Elm", "Lisp", "Clojure" ]
                        |> Expect.equal True
            , test "Three-item list with Elm as second item is exciting" <|
                \_ ->
                    excitingList [ "Java", "Elm", "C#" ]
                        |> Expect.equal True
            , test "Three-item list with Elm as third item is not exciting" <|
                \_ ->
                    excitingList [ "Julia", "Assembly", "Elm" ]
                        |> Expect.equal False
            , test "Four-item list with Elm as first item is exciting" <|
                \_ ->
                    excitingList [ "Elm", "C", "C++", "C#" ]
                        |> Expect.equal True
            , test "Four-item list with Elm as second item is not exciting" <|
                \_ ->
                    excitingList [ "Erlang", "Elm", "C#", "Scheme" ]
                        |> Expect.equal False
            , test "Four-item list with Elm as third item is not exciting" <|
                \_ ->
                    excitingList [ "Erlang", "C#", "Elm", "Scheme" ]
                        |> Expect.equal False
            , test "Four-item list with Elm as fourth item is not exciting" <|
                \_ ->
                    excitingList [ "Erlang", "C#", "Scheme", "Elm" ]
                        |> Expect.equal False
            ]
        , describe "Annalyn infiltration"
            [ test
                "Cannot execute fast attack if knight is awake"
              <|
                \_ ->
                    let
                        knightIsAwake =
                            True
                    in
                    canFastAttack knightIsAwake
                        |> Expect.equal False
            , test "Can execute fast attack if knight is sleeping" <|
                \_ ->
                    let
                        knightIsAwake =
                            False
                    in
                    canFastAttack knightIsAwake
                        |> Expect.equal True
            , test "Cannot spy if everyone is sleeping" <|
                \_ ->
                    let
                        knightIsAwake =
                            False

                        archerIsAwake =
                            False

                        prisonerIsAwake =
                            False
                    in
                    canSpy knightIsAwake archerIsAwake prisonerIsAwake
                        |> Expect.equal False
            , test "Can spy if everyone but knight is sleeping" <|
                \_ ->
                    let
                        knightIsAwake =
                            True

                        archerIsAwake =
                            False

                        prisonerIsAwake =
                            False
                    in
                    canSpy knightIsAwake archerIsAwake prisonerIsAwake
                        |> Expect.equal True
            , test "Can spy if everyone but archer is sleeping" <|
                \_ ->
                    let
                        knightIsAwake =
                            False

                        archerIsAwake =
                            True

                        prisonerIsAwake =
                            False
                    in
                    canSpy knightIsAwake archerIsAwake prisonerIsAwake
                        |> Expect.equal True
            , test "Can spy if everyone but prisoner is sleeping" <|
                \_ ->
                    let
                        knightIsAwake =
                            False

                        archerIsAwake =
                            False

                        prisonerIsAwake =
                            True
                    in
                    canSpy knightIsAwake archerIsAwake prisonerIsAwake
                        |> Expect.equal True
            , test "Can spy if only knight is sleeping" <|
                \_ ->
                    let
                        knightIsAwake =
                            False

                        archerIsAwake =
                            True

                        prisonerIsAwake =
                            True
                    in
                    canSpy knightIsAwake archerIsAwake prisonerIsAwake
                        |> Expect.equal True
            , test "Can spy if only archer is sleeping" <|
                \_ ->
                    let
                        knightIsAwake =
                            True

                        archerIsAwake =
                            False

                        prisonerIsAwake =
                            True
                    in
                    canSpy knightIsAwake archerIsAwake prisonerIsAwake
                        |> Expect.equal True
            , test "Can spy if only prisoner is sleeping" <|
                \_ ->
                    let
                        knightIsAwake =
                            True

                        archerIsAwake =
                            True

                        prisonerIsAwake =
                            False
                    in
                    canSpy knightIsAwake archerIsAwake prisonerIsAwake
                        |> Expect.equal True
            , test "Can spy if everyone is awake" <|
                \_ ->
                    let
                        knightIsAwake =
                            True

                        archerIsAwake =
                            True

                        prisonerIsAwake =
                            True
                    in
                    canSpy knightIsAwake archerIsAwake prisonerIsAwake
                        |> Expect.equal True
            , test "Can signal prisoner if archer is sleeping and prisoner is awake" <|
                \_ ->
                    let
                        archerIsAwake =
                            False

                        prisonerIsAwake =
                            True
                    in
                    canSignalPrisoner archerIsAwake prisonerIsAwake
                        |> Expect.equal True
            , test "Cannot signal prisoner if archer is awake and prisoner is sleeping" <|
                \_ ->
                    let
                        archerIsAwake =
                            True

                        prisonerIsAwake =
                            False
                    in
                    canSignalPrisoner archerIsAwake prisonerIsAwake
                        |> Expect.equal False
            , test "Cannot signal prisoner if archer and prisoner are both sleeping" <|
                \_ ->
                    let
                        archerIsAwake =
                            False

                        prisonerIsAwake =
                            False
                    in
                    canSignalPrisoner archerIsAwake prisonerIsAwake
                        |> Expect.equal False
            , test "Cannot signal prisoner if archer and prisoner are both awake" <|
                \_ ->
                    let
                        archerIsAwake =
                            True

                        prisonerIsAwake =
                            True
                    in
                    canSignalPrisoner archerIsAwake prisonerIsAwake
                        |> Expect.equal False
            , test "Cannot release prisoner if everyone is awake and pet dog is present" <|
                \_ ->
                    let
                        knightIsAwake =
                            True

                        archerIsAwake =
                            True

                        prisonerIsAwake =
                            True

                        petDogIsPresent =
                            True
                    in
                    canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
                        |> Expect.equal False
            , test "Cannot release prisoner if everyone is awake and pet dog is absent" <|
                \_ ->
                    let
                        knightIsAwake =
                            True

                        archerIsAwake =
                            True

                        prisonerIsAwake =
                            True

                        petDogIsPresent =
                            False
                    in
                    canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
                        |> Expect.equal False
            , test "Can release prisoner if everyone is asleep and pet dog is present" <|
                \_ ->
                    let
                        knightIsAwake =
                            False

                        archerIsAwake =
                            False

                        prisonerIsAwake =
                            False

                        petDogIsPresent =
                            True
                    in
                    canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
                        |> Expect.equal True
            , test "Cannot release prisoner if everyone is asleep and pet dog is absent" <|
                \_ ->
                    let
                        knightIsAwake =
                            False

                        archerIsAwake =
                            False

                        prisonerIsAwake =
                            False

                        petDogIsPresent =
                            False
                    in
                    canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
                        |> Expect.equal False
            , test "Can release prisoner if only prisoner is awake and pet dog is present" <|
                \_ ->
                    let
                        knightIsAwake =
                            False

                        archerIsAwake =
                            False

                        prisonerIsAwake =
                            True

                        petDogIsPresent =
                            True
                    in
                    canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
                        |> Expect.equal True
            , test "Can release prisoner if only prisoner is awake and pet dog is absent" <|
                \_ ->
                    let
                        knightIsAwake =
                            False

                        archerIsAwake =
                            False

                        prisonerIsAwake =
                            True

                        petDogIsPresent =
                            False
                    in
                    canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
                        |> Expect.equal True
            , test "Cannot release prisoner if only archer is awake and pet dog is present" <|
                \_ ->
                    let
                        knightIsAwake =
                            False

                        archerIsAwake =
                            True

                        prisonerIsAwake =
                            False

                        petDogIsPresent =
                            True
                    in
                    canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
                        |> Expect.equal False
            , test "Cannot release prisoner if only archer is awake and pet dog is absent" <|
                \_ ->
                    let
                        knightIsAwake =
                            False

                        archerIsAwake =
                            True

                        prisonerIsAwake =
                            False

                        petDogIsPresent =
                            False
                    in
                    canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
                        |> Expect.equal False
            , test "Can release prisoner if only knight is awake and pet dog is present" <|
                \_ ->
                    let
                        knightIsAwake =
                            True

                        archerIsAwake =
                            False

                        prisonerIsAwake =
                            False

                        petDogIsPresent =
                            True
                    in
                    canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
                        |> Expect.equal True
            , test "Cannot release prisoner if only knight is awake and pet dog is absent" <|
                \_ ->
                    let
                        knightIsAwake =
                            True

                        archerIsAwake =
                            False

                        prisonerIsAwake =
                            False

                        petDogIsPresent =
                            False
                    in
                    canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
                        |> Expect.equal False
            , test "Cannot release prisoner if only knight is asleep and pet dog is present" <|
                \_ ->
                    let
                        knightIsAwake =
                            False

                        archerIsAwake =
                            True

                        prisonerIsAwake =
                            True

                        petDogIsPresent =
                            True
                    in
                    canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
                        |> Expect.equal False
            , test "Cannot release prisoner if only knight is asleep and pet dog is absent" <|
                \_ ->
                    let
                        knightIsAwake =
                            False

                        archerIsAwake =
                            True

                        prisonerIsAwake =
                            True

                        petDogIsPresent =
                            False
                    in
                    canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
                        |> Expect.equal False
            , test "Can release prisoner if only archer is asleep and pet dog is present" <|
                \_ ->
                    let
                        knightIsAwake =
                            True

                        archerIsAwake =
                            False

                        prisonerIsAwake =
                            True

                        petDogIsPresent =
                            True
                    in
                    canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
                        |> Expect.equal True
            , test "Cannot release prisoner if only archer is asleep and pet dog is absent" <|
                \_ ->
                    let
                        knightIsAwake =
                            True

                        archerIsAwake =
                            False

                        prisonerIsAwake =
                            True

                        petDogIsPresent =
                            False
                    in
                    canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
                        |> Expect.equal False
            , test "Cannot release prisoner if only prisoner is asleep and pet dog is present" <|
                \_ ->
                    let
                        knightIsAwake =
                            True

                        archerIsAwake =
                            True

                        prisonerIsAwake =
                            False

                        petDogIsPresent =
                            True
                    in
                    canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
                        |> Expect.equal False
            , test "Cannot release prisoner if only prisoner is asleep and pet dog is absent" <|
                \_ ->
                    let
                        knightIsAwake =
                            True

                        archerIsAwake =
                            True

                        prisonerIsAwake =
                            False

                        petDogIsPresent =
                            False
                    in
                    canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
                        |> Expect.equal False
            , test "Annalyn does 12 damage if undetected" <|
                \_ ->
                    let
                        annalynIsDetected =
                            False
                    in
                    stealthAttackDamage annalynIsDetected
                        |> Expect.equal 12
            , test "Annalyn does 7 damage if detected" <|
                \_ ->
                    let
                        annalynIsDetected =
                            True
                    in
                    stealthAttackDamage annalynIsDetected
                        |> Expect.equal 7
            ]
        , describe "Bandwagoner"
            [ test
                "has Coach type alias with correct fields in correct order"
              <|
                \_ ->
                    Coach "Steve Kerr" True
                        |> Expect.equal { name = "Steve Kerr", formerPlayer = True }
            , test "has Stats type alias with correct fields in correct order" <|
                \_ ->
                    Stats 55 27
                        |> Expect.equal { wins = 55, losses = 27 }
            , test "has Team type alias with correct fields in correct order" <|
                \_ ->
                    let
                        coach =
                            Coach "Red Auerbach" False

                        stats =
                            Stats 58 22

                        team =
                            { name = "Boston Celtics", coach = coach, stats = stats }
                    in
                    Expect.equal team (Team "Boston Celtics" coach stats)
            , test "createTeam creates a Team structural type" <|
                \_ ->
                    let
                        coach =
                            Coach "Red Auerbach" False

                        stats =
                            Stats 58 22

                        team =
                            createTeam "Boston Celtics" stats coach
                    in
                    Expect.equal team (Team "Boston Celtics" coach stats)
            , test "can replace coach for a team" <|
                \_ ->
                    let
                        coach =
                            Coach "Willis Reed" True

                        newCoach =
                            Coach "Red Holzman" True

                        stats =
                            Stats 6 8

                        team =
                            Team "New York Knicks" coach stats

                        newTeam =
                            replaceCoach newCoach team
                    in
                    Expect.equal newTeam (Team "New York Knicks" newCoach stats)
            , test "should root for teams that have more wins than losses" <|
                \_ ->
                    Team "" (Coach "" True) (Stats 1 0)
                        |> rootForTeam
                        |> Expect.equal True
            , test "should not root for teams that lose more than they win" <|
                \_ ->
                    Team "" (Coach "" True) (Stats 43 44)
                        |> rootForTeam
                        |> Expect.equal False
            , test "should not root for teams that have equal losses and wins" <|
                \_ ->
                    Team "" (Coach "" True) (Stats 143 143)
                        |> rootForTeam
                        |> Expect.equal False
            , test "should root for extended teams that have more wins than losses" <|
                \_ ->
                    { name = "", coach = Coach "" True, stats = Stats 1 0, someOtherField = "" }
                        |> rootForTeam
                        |> Expect.equal True

            -- valentine
            , test "board game rated no" <|
                \_ ->
                    rateActivity BoardGame
                        |> Expect.equal No
            , test "chill rated no" <|
                \_ ->
                    rateActivity Chill
                        |> Expect.equal No
            , test "crime movie rated no" <|
                \_ ->
                    rateActivity (Movie Crime)
                        |> Expect.equal No
            , test "horror movie rated no" <|
                \_ ->
                    rateActivity (Movie Horror)
                        |> Expect.equal No
            , test "romance movie rated yes" <|
                \_ ->
                    rateActivity (Movie Romance)
                        |> Expect.equal Yes
            , test "thriller movie rated no" <|
                \_ ->
                    rateActivity (Movie Thriller)
                        |> Expect.equal No
            , test "korean restaurant rated no" <|
                \_ ->
                    rateActivity (Restaurant Korean)
                        |> Expect.equal Yes
            , test "turkish restaurant rated maybe" <|
                \_ ->
                    rateActivity (Restaurant Turkish)
                        |> Expect.equal Maybe
            ]
        , describe "lasagna"
            [ test "For a 3-layers lasagna started 10 minutes ago, there are 36 minutes remaining" <|
                \_ ->
                    remainingTimeInMinutes 3 10
                        |> Expect.equal ((3 * 2) + 40 - 10)
            , test "For a 6-layers lasagna started 30 minutes ago, there are 22 minutes remaining" <|
                \_ ->
                    remainingTimeInMinutes 6 30
                        |> Expect.equal ((6 * 2) + 40 - 30)
            ]
        , describe "tisbury"
            [ test "placeLocationToTreasureLocation should convert PlaceLocation to TreasureLocation" <|
                \_ ->
                    placeLocationToTreasureLocation ( 'C', 1 )
                        |> Expect.equal ( 1, 'C' )
            , test "1,F is not at Seaside Cottages" <|
                \_ ->
                    treasureLocationMatchesPlaceLocation ( 'C', 1 ) ( 1, 'F' )
                        |> Expect.equal False
            , test "1, F is at Aqua Lagoon" <|
                \_ ->
                    treasureLocationMatchesPlaceLocation ( 'F', 1 ) ( 1, 'F' )
                        |> Expect.equal True
            , test "places should know how many treasures are available" <|
                \_ ->
                    countPlaceTreasures
                        ( "Aqua Lagoon (Island of Mystery)", ( 'F', 1 ) )
                        [ ( "Amethyst Octopus", ( 1, 'F' ) )
                        , ( "Scrimshaw Whale's Tooth", ( 1, 'F' ) )
                        ]
                        |> Expect.equal 2
            , test "can swap Amethyst Octopus for Crystal Crab at Stormy Breakwater" <|
                \_ ->
                    specialCaseSwapPossible
                        ( "Amethyst Octopus", ( 1, 'F' ) )
                        ( "Stormy Breakwater", ( 'B', 5 ) )
                        ( "Crystal Crab", ( 6, 'A' ) )
                        |> Expect.equal True
            , test "can swap Amethyst Octopus for Glass Starfish at Stormy Breakwater" <|
                \_ ->
                    specialCaseSwapPossible
                        ( "Amethyst Octopus", ( 1, 'F' ) )
                        ( "Stormy Breakwater", ( 'B', 5 ) )
                        ( "Glass Starfish", ( 6, 'D' ) )
                        |> Expect.equal True
            , test "cannot swap Amethyst Octopus for Angry Monkey Figurine at Stormy Breakwater" <|
                \_ ->
                    specialCaseSwapPossible
                        ( "Amethyst Octopus", ( 1, 'F' ) )
                        ( "Stormy Breakwater", ( 'B', 5 ) )
                        ( "Angry Monkey Figurine", ( 5, 'B' ) )
                        |> Expect.equal False
            , test "can swap Vintage Pirate Hat for Model Ship in Large Bottle at Harbor Managers Office" <|
                \_ ->
                    specialCaseSwapPossible
                        ( "Vintage Pirate Hat", ( 7, 'E' ) )
                        ( "Harbor Managers Office", ( 'A', 8 ) )
                        ( "Model Ship in Large Bottle", ( 8, 'A' ) )
                        |> Expect.equal True
            , test "can swap Vintage Pirate Hat for Antique Glass Fishnet Float at Harbor Managers Office" <|
                \_ ->
                    specialCaseSwapPossible
                        ( "Vintage Pirate Hat", ( 7, 'E' ) )
                        ( "Harbor Managers Office", ( 'A', 8 ) )
                        ( "Antique Glass Fishnet Float", ( 3, 'D' ) )
                        |> Expect.equal True
            , test "cannot swap Vintage Pirate Hat for Carved Wooden Elephant at Harbor Managers Office" <|
                \_ ->
                    specialCaseSwapPossible
                        ( "Vintage Pirate Hat", ( 7, 'E' ) )
                        ( "Harbor Managers Office", ( 'A', 8 ) )
                        ( "Carved Wooden Elephant", ( 8, 'C' ) )
                        |> Expect.equal False
            , test "can swap Brass Spyglass for Robot Parrot at Abandoned Lighthouse" <|
                \_ ->
                    specialCaseSwapPossible
                        ( "Brass Spyglass", ( 4, 'B' ) )
                        ( "Abandoned Lighthouse", ( 'B', 4 ) )
                        ( "Robot Parrot", ( 1, 'C' ) )
                        |> Expect.equal True
            , test "cannot swap Vintage Pirate Hat for Model Ship in Large Bottle at Old Schooner" <|
                \_ ->
                    specialCaseSwapPossible
                        ( "Vintage Pirate Hat", ( 7, 'E' ) )
                        ( "Old Schooner", ( 'A', 6 ) )
                        ( "Model Ship in Large Bottle", ( 8, 'A' ) )
                        |> Expect.equal False
            ]
        , describe "role playing game"
            [ test "Introducing someone with their name" <|
                \() ->
                    Expect.equal (introduce { name = Just "Gandalf", level = 1, health = 42, mana = Nothing }) "Gandalf"
            , test "Introducing an unidentified player should return 'Mighty Magician'" <|
                \() ->
                    Expect.equal (introduce { name = Nothing, level = 1, health = 42, mana = Nothing }) "Mighty Magician"
            , test "Attempting to revive a player that is alive should return Nothing" <|
                \() ->
                    Expect.equal (revive { name = Nothing, level = 12, health = 42, mana = Just 7 }) Nothing
            , test "Reviving a low level player resets its health to 100" <|
                \() ->
                    Expect.equal (revive { name = Nothing, level = 3, health = 0, mana = Nothing })
                        (Just { name = Nothing, level = 3, health = 100, mana = Nothing })
            , test "Reviving a high level player resets both its health and mana" <|
                \() ->
                    Expect.equal (revive { name = Nothing, level = 10, health = 0, mana = Just 14 })
                        (Just { name = Nothing, level = 10, health = 100, mana = Just 100 })
            , test "Casting a spell spends double the mana" <|
                \() ->
                    Expect.equal (castSpell 9 { name = Nothing, level = 10, health = 69, mana = Just 20 })
                        ( { name = Nothing, level = 10, health = 69, mana = Just 11 }, 18 )
            , test "Attempting to cast a spell with insufficient mana does nothing" <|
                \() ->
                    Expect.equal (castSpell 39 { name = Nothing, level = 10, health = 69, mana = Just 20 })
                        ( { name = Nothing, level = 10, health = 69, mana = Just 20 }, 0 )
            , test "Attempting to cast a spell without a mana pool decreases the player's health" <|
                \() ->
                    Expect.equal (castSpell 7 { name = Nothing, level = 5, health = 58, mana = Nothing })
                        ( { name = Nothing, level = 5, health = 51, mana = Nothing }, 0 )
            , test "A player's health cannot go below 0" <|
                \() ->
                    Expect.equal (castSpell 12 { name = Nothing, level = 5, health = 6, mana = Nothing })
                        ( { name = Nothing, level = 5, health = 0, mana = Nothing }, 0 )
            ]
        , describe "TicketPlease "
            [ describe "Task 1: TicketPlease.emptyComment"
                [ test "emptyComment should detect an empty comment" <|
                    \() ->
                        emptyComment ( User "Alice", "" )
                            |> Expect.true "Expected the comment to be empty"
                , test "emptyComment should detect an non-empty comment" <|
                    \() ->
                        emptyComment ( User "Alice", "hello" )
                            |> Expect.false "Expected the comment to contain something"
                , test "emptyComment can be used in a filter" <|
                    \() ->
                        [ ( User "Alice", "hello" )
                        , ( User "Bob", "" )
                        , ( User "Alice", "hello?" )
                        , ( User "Bob", "hi!" )
                        ]
                            |> List.filter (emptyComment >> not)
                            |> Expect.equalLists
                                [ ( User "Alice", "hello" )
                                , ( User "Alice", "hello?" )
                                , ( User "Bob", "hi!" )
                                ]
                ]
            , describe "Task 2: TicketPlease.numberOfCreatorComments"
                [ test "numberOfCreatorComments with no comment" <|
                    \() ->
                        Ticket { newTicket | createdBy = ( User "John", 1 ), comments = [] }
                            |> numberOfCreatorComments
                            |> Expect.equal 0
                , test "numberOfCreatorComments with no comment from creator" <|
                    \() ->
                        Ticket
                            { newTicket
                                | createdBy = ( User "John", 1 )
                                , comments = [ ( User "Alice", "How may I help you?" ) ]
                            }
                            |> numberOfCreatorComments
                            |> Expect.equal 0
                , test "numberOfCreatorComments with one comment from creator" <|
                    \() ->
                        Ticket
                            { newTicket
                                | createdBy = ( User "Jack", 2 )
                                , comments = [ ( User "Jack", "I can't connect to the wifi." ) ]
                            }
                            |> numberOfCreatorComments
                            |> Expect.equal 1
                , test "numberOfCreatorComments with several comments" <|
                    \() ->
                        Ticket
                            { newTicket
                                | createdBy = ( User "Steve", 1 )
                                , comments =
                                    [ ( User "Roy", "Hello, IT." )
                                    , ( User "Steve", "My computer refuses to print anything." )
                                    , ( User "Roy", "Have you tried turning it off and on again?" )
                                    , ( User "Steve", "Is that a reference to something?" )
                                    ]
                            }
                            |> numberOfCreatorComments
                            |> Expect.equal 2
                ]
            , describe "Task 3: TicketPlease.assignedToDevTeam"
                [ test "assignedToDevTeam with no one assigned" <|
                    \() ->
                        Ticket { newTicket | assignedTo = Nothing }
                            |> assignedToDevTeam
                            |> Expect.false "Expected unassigned ticket"
                , test "assignedToDevTeam with ticket assigned to non-dev team" <|
                    \() ->
                        Ticket { newTicket | assignedTo = Just (User "Roy") }
                            |> assignedToDevTeam
                            |> Expect.false "Expected ticket not assigned to dev team"
                , test "assignedToDevTeam with ticket assigned to Alice from dev team" <|
                    \() ->
                        Ticket { newTicket | assignedTo = Just (User "Alice") }
                            |> assignedToDevTeam
                            |> Expect.true "Expected ticket assigned to Alice from dev team"
                , test "assignedToDevTeam with ticket assigned to Bob from dev team" <|
                    \() ->
                        Ticket { newTicket | assignedTo = Just (User "Bob") }
                            |> assignedToDevTeam
                            |> Expect.true "Expected ticket assigned to Bob from dev team"
                , test "assignedToDevTeam with ticket assigned to Charlie from dev team" <|
                    \() ->
                        Ticket { newTicket | assignedTo = Just (User "Charlie") }
                            |> assignedToDevTeam
                            |> Expect.true "Expected ticket assigned to Charlie from dev team"
                ]
            , describe "Task 4: TicketPlease.assignTicketTo"
                [ test "assign new, unassigned ticket to Roy" <|
                    \() ->
                        Ticket { newTicket | status = New, assignedTo = Nothing }
                            |> assignTicketTo (User "Roy")
                            |> Expect.equal
                                (Ticket { newTicket | status = InProgress, assignedTo = Just (User "Roy") })
                , test "assign new, assigned ticket to Moss" <|
                    \() ->
                        Ticket { newTicket | status = New, assignedTo = Just (User "Alice") }
                            |> assignTicketTo (User "Moss")
                            |> Expect.equal
                                (Ticket { newTicket | status = InProgress, assignedTo = Just (User "Moss") })
                , test "assign in progress, unassigned ticket to Roy" <|
                    \() ->
                        Ticket { newTicket | status = InProgress, assignedTo = Nothing }
                            |> assignTicketTo (User "Roy")
                            |> Expect.equal
                                (Ticket { newTicket | status = InProgress, assignedTo = Just (User "Roy") })
                , test "assign in progress, assigned ticket to Moss" <|
                    \() ->
                        Ticket { newTicket | status = InProgress, assignedTo = Just (User "Alice") }
                            |> assignTicketTo (User "Moss")
                            |> Expect.equal
                                (Ticket { newTicket | status = InProgress, assignedTo = Just (User "Moss") })
                , test "assign resolved, unassigned ticket to Roy" <|
                    \() ->
                        Ticket { newTicket | status = Resolved, assignedTo = Nothing }
                            |> assignTicketTo (User "Roy")
                            |> Expect.equal
                                (Ticket { newTicket | status = Resolved, assignedTo = Just (User "Roy") })
                , test "assign resolved, assigned ticket to Moss" <|
                    \() ->
                        Ticket { newTicket | status = Resolved, assignedTo = Just (User "Alice") }
                            |> assignTicketTo (User "Moss")
                            |> Expect.equal
                                (Ticket { newTicket | status = Resolved, assignedTo = Just (User "Moss") })
                , test "assign closed, unassigned ticket to Roy" <|
                    \() ->
                        Ticket { newTicket | status = Closed, assignedTo = Nothing }
                            |> assignTicketTo (User "Roy")
                            |> Expect.equal
                                (Ticket { newTicket | status = Closed, assignedTo = Just (User "Roy") })
                , test "assign closed, assigned ticket to Moss" <|
                    \() ->
                        Ticket { newTicket | status = Closed, assignedTo = Just (User "Alice") }
                            |> assignTicketTo (User "Moss")
                            |> Expect.equal
                                (Ticket { newTicket | status = Closed, assignedTo = Just (User "Moss") })
                , test "unsuccesfulfy assign archived, unassigned ticket to Roy" <|
                    \() ->
                        Ticket { newTicket | status = Archived, assignedTo = Nothing }
                            |> assignTicketTo (User "Roy")
                            |> Expect.equal
                                (Ticket { newTicket | status = Archived, assignedTo = Nothing })
                , test "unsuccesfulfy assign archived, assigned ticket to Moss" <|
                    \() ->
                        Ticket { newTicket | status = Archived, assignedTo = Just (User "Alice") }
                            |> assignTicketTo (User "Moss")
                            |> Expect.equal
                                (Ticket { newTicket | status = Archived, assignedTo = Just (User "Alice") })
                ]
            ]
        ]


newTicket : { status : Status, createdBy : ( User, Int ), assignedTo : Maybe User, comments : List ( User, String ) }
newTicket =
    { status = New
    , createdBy = ( User "John", 1 )
    , assignedTo = Just (User "Bob")
    , comments = [ ( User "John", "ping?" ), ( User "Bob", "pong!" ) ]
    }
