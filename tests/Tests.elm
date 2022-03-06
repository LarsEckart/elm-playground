module Tests exposing (tests)

import AnnalynsInfiltration exposing (canFastAttack, canFreePrisoner, canSignalPrisoner, canSpy, stealthAttackDamage)
import Bandwagoner exposing (..)
import BettysBikeShop exposing (..)
import Expect exposing (FloatingPointTolerance(..))
import Test exposing (..)
import TracksOnTracksOnTracks exposing (..)


tests : Test
tests =
    describe "BettysBikeShop"
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

        -- Annalyn infiltration
        , test "Cannot execute fast attack if knight is awake" <|
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

        --Bandwagoner
        , test
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
        ]
