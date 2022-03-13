module RolePlayingGame exposing (Player, castSpell, introduce, revive)


type alias Player =
    { name : Maybe String
    , level : Int
    , health : Int
    , mana : Maybe Int
    }


introduce : Player -> String
introduce { name } =
    case name of
        Nothing ->
            "Mighty Magician"

        Just n ->
            n


revive : Player -> Maybe Player
revive player =
    case player.health of
        0 ->
            let
                newMana =
                    if player.level < 10 then
                        Nothing

                    else
                        Just 100
            in
            Just { name = player.name, level = player.level, health = 100, mana = newMana }

        _ ->
            Nothing


castSpell : Int -> Player -> ( Player, Int )
castSpell manaCost player =
    case player.mana of
        Nothing ->
            ( { player | health = decreasePlayerHealth player.health manaCost }, 0 )

        Just manaPool ->
            if manaPool < manaCost then
                ( player, 0 )

            else
                let
                    newMana =
                        Just (manaPool - manaCost)
                in
                ( { player | mana = newMana }, manaCost * 2 )


decreasePlayerHealth : Int -> Int -> Int
decreasePlayerHealth health manaCost =
    let
        newHealth =
            health - manaCost
    in
    if newHealth < 0 then
        0

    else
        newHealth
