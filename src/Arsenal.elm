module Arsenal exposing (Arsenal(..), ArsenalSlotType, accessArsenal, addWeaponToArsenal, arsenalSlotType1, arsenalSlotType2, arsenalSlotType3, arsenalSlotType4, getWeaponBySlotType, isArsenalFull, makeEmptyArsenal)

import Utils exposing (flattenMaybe, mapCaseOf)
import Weapon exposing (Weapon)


type Arsenal
    = Arsenal
        { slot1 : Maybe Weapon
        , slot2 : Maybe Weapon
        , slot3 : Maybe Weapon
        , slot4 : Maybe Weapon
        }


type ArsenalSlotType
    = ArsenalSlotType String


arsenalSlotType1 : ArsenalSlotType
arsenalSlotType1 =
    ArsenalSlotType "slot1"


arsenalSlotType2 : ArsenalSlotType
arsenalSlotType2 =
    ArsenalSlotType "slot2"


arsenalSlotType3 : ArsenalSlotType
arsenalSlotType3 =
    ArsenalSlotType "slot3"


arsenalSlotType4 : ArsenalSlotType
arsenalSlotType4 =
    ArsenalSlotType "slot4"


accessArsenal : ArsenalSlotType -> Arsenal -> ( Maybe Weapon, Arsenal )
accessArsenal slotType (Arsenal arsenal) =
    if slotType == arsenalSlotType1 then
        ( arsenal.slot1, Arsenal { arsenal | slot1 = Nothing } )

    else if slotType == arsenalSlotType2 then
        ( arsenal.slot2, Arsenal { arsenal | slot2 = Nothing } )

    else if slotType == arsenalSlotType3 then
        ( arsenal.slot3, Arsenal { arsenal | slot3 = Nothing } )

    else
        ( arsenal.slot4, Arsenal { arsenal | slot4 = Nothing } )


addWeaponToArsenal : Weapon -> Arsenal -> Arsenal
addWeaponToArsenal weapon (Arsenal arsenal) =
    [ ( arsenal.slot1, Arsenal { arsenal | slot1 = Just weapon } )
    , ( arsenal.slot2, Arsenal { arsenal | slot2 = Just weapon } )
    , ( arsenal.slot3, Arsenal { arsenal | slot3 = Just weapon } )
    , ( arsenal.slot4, Arsenal { arsenal | slot4 = Just weapon } )
    ]
        |> mapCaseOf ((==) Nothing)
        |> Maybe.withDefault (Arsenal arsenal)


getWeaponBySlotType : ArsenalSlotType -> Arsenal -> Maybe Weapon
getWeaponBySlotType slotType (Arsenal arsenal) =
    [ ( arsenalSlotType1, arsenal.slot1 )
    , ( arsenalSlotType2, arsenal.slot2 )
    , ( arsenalSlotType3, arsenal.slot3 )
    , ( arsenalSlotType4, arsenal.slot4 )
    ]
        |> mapCaseOf ((==) slotType)
        |> flattenMaybe


isArsenalFull : Arsenal -> Bool
isArsenalFull (Arsenal arsenal) =
    arsenal.slot1 /= Nothing && arsenal.slot2 /= Nothing && arsenal.slot3 /= Nothing && arsenal.slot4 /= Nothing


makeEmptyArsenal : () -> Arsenal
makeEmptyArsenal =
    always (Arsenal { slot1 = Nothing, slot2 = Nothing, slot3 = Nothing, slot4 = Nothing })
