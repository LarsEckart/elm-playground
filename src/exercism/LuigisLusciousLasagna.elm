module LuigisLusciousLasagna exposing (remainingTimeInMinutes)


remainingTimeInMinutes : Int -> Int -> Int
remainingTimeInMinutes layers minutesSinceStart =
    let
        expectedMinutesInOven =
            40

        preparationTimeInMinutes =
            layers * 2
    in
    expectedMinutesInOven + preparationTimeInMinutes - minutesSinceStart
