module Bandwagoner exposing (..)

-- TODO: please define a type alias for the 'Coach' record
-- TODO: please define the 'Stats' record
-- TODO: please define the 'Team' record


type alias Coach =
    { name : String
    , formerPlayer : Bool
    }


type alias Stats =
    { wins : Int, losses : Int }


type alias Team =
    { name : String, coach : Coach, stats : Stats }


replaceCoach newCoach team =
    { team | coach = newCoach }


createTeam name stats coach =
    Team name coach stats



-- TODO: use an extensible record to accept any record that has a `stats: Stats` field
-- TODO: use pattern matching in the function parameter to get the `stats` field from the record


rootForTeam : { x | stats : Stats } -> Bool
rootForTeam team =
    team.stats.wins > team.stats.losses
