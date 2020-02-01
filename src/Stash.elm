module Stash exposing (Stash(..), StashSlotType, access, addToStash, getPosition, getSlotTypeByPosition, makeEmptyStash, makeStash, populateStashFromList, stashSlotType1, stashSlotType2, stashSlotType3, stashSlotType4)

import Physics exposing (Position, arePositionsHorizontallyEquivalent)
import Utils exposing (flattenMaybe, mapCaseOf)
import WeaponLocation exposing (WeaponLocation)


type alias Stash_ =
    { slot1 : Maybe WeaponLocation
    , slot2 : Maybe WeaponLocation
    , slot3 : Maybe WeaponLocation
    , slot4 : Maybe WeaponLocation
    }


type Stash
    = Stash Stash_


type StashSlotType
    = StashSlotType String


stashSlotType1 : StashSlotType
stashSlotType1 =
    StashSlotType "slot1"


stashSlotType2 : StashSlotType
stashSlotType2 =
    StashSlotType "slot2"


stashSlotType3 : StashSlotType
stashSlotType3 =
    StashSlotType "slot3"


stashSlotType4 : StashSlotType
stashSlotType4 =
    StashSlotType "slot4"


access : StashSlotType -> Stash -> ( Maybe WeaponLocation, Stash )
access slotType (Stash stash) =
    [ ( stashSlotType1, ( stash.slot1, Stash { stash | slot1 = Nothing } ) )
    , ( stashSlotType2, ( stash.slot2, Stash { stash | slot2 = Nothing } ) )
    , ( stashSlotType3, ( stash.slot3, Stash { stash | slot3 = Nothing } ) )
    , ( stashSlotType4, ( stash.slot4, Stash { stash | slot4 = Nothing } ) )
    ]
        |> mapCaseOf ((==) slotType)
        |> Maybe.withDefault ( Nothing, Stash stash )


addToStash : WeaponLocation -> Stash -> Stash
addToStash weaponLocation (Stash stash) =
    [ ( stash.slot1, Stash { stash | slot1 = Just weaponLocation } )
    , ( stash.slot2, Stash { stash | slot2 = Just weaponLocation } )
    , ( stash.slot3, Stash { stash | slot3 = Just weaponLocation } )
    , ( stash.slot4, Stash { stash | slot4 = Just weaponLocation } )
    ]
        |> mapCaseOf ((==) Nothing)
        |> Maybe.withDefault (Stash stash)


getPosition : StashSlotType -> Stash -> Maybe Position
getPosition slotType (Stash stash) =
    [ ( stashSlotType1, stash.slot1 )
    , ( stashSlotType2, stash.slot2 )
    , ( stashSlotType3, stash.slot3 )
    , ( stashSlotType4, stash.slot4 )
    ]
        |> mapCaseOf ((==) slotType)
        |> flattenMaybe
        |> Maybe.map WeaponLocation.toPosition


getSlotTypeByPosition : Stash -> Position -> Maybe StashSlotType
getSlotTypeByPosition (Stash { slot1, slot2, slot3, slot4 }) position =
    let
        predicate =
            Maybe.map WeaponLocation.toPosition
                >> Maybe.map (arePositionsHorizontallyEquivalent position)
                >> Maybe.withDefault False
    in
    [ ( slot1, Just stashSlotType1 )
    , ( slot2, Just stashSlotType2 )
    , ( slot3, Just stashSlotType3 )
    , ( slot4, Just stashSlotType4 )
    ]
        |> mapCaseOf predicate
        |> Maybe.withDefault Nothing


isFull : Stash_ -> Bool
isFull { slot1, slot2, slot3, slot4 } =
    List.all ((/=) Nothing) [ slot1, slot2, slot3, slot4 ]


makeEmptyStash : () -> Stash
makeEmptyStash =
    always (makeStash Nothing Nothing Nothing Nothing)


makeStash : Maybe WeaponLocation -> Maybe WeaponLocation -> Maybe WeaponLocation -> Maybe WeaponLocation -> Stash
makeStash wl1 wl2 wl3 wl4 =
    -- this will more useful when save and continue functionality is implemented
    Stash { slot1 = wl1, slot2 = wl2, slot3 = wl3, slot4 = wl4 }


populateStashFromList : List WeaponLocation -> Stash -> Stash
populateStashFromList weaponLocations (Stash stash) =
    case ( isFull stash, weaponLocations ) of
        ( False, wl :: wls ) ->
            populateStashFromList wls (addToStash wl (Stash stash))

        _ ->
            Stash stash
