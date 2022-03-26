module TracksOnTracksOnTracks exposing (..)


newList : List String
newList =
    []


existingList : List String
existingList =
    [ "Elm", "Clojure", "Haskell" ]


addLanguage : String -> List String -> List String
addLanguage language languages =
    language :: languages


countLanguages : List String -> Int
countLanguages languages =
    List.length languages


reverseList : List String -> List String
reverseList languages =
    List.reverse languages


excitingList : List String -> Bool
excitingList languages =
    case languages of
        [] ->
            False

        [ "Elm" ] ->
            True

        x :: xs ->
            x == "Elm" || secondIsElm xs && List.length xs <= 2


secondIsElm : List String -> Bool
secondIsElm list =
    case list of
        f :: s ->
            f == "Elm"

        [] ->
            False
