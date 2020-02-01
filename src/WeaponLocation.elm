module WeaponLocation exposing (WeaponLocation(..), makeWeaponLocation, toPosition)

import Physics exposing (Position)


type WeaponLocation
    = WeaponLocation Position


makeWeaponLocation : Position -> WeaponLocation
makeWeaponLocation position =
    WeaponLocation position


toPosition : WeaponLocation -> Position
toPosition (WeaponLocation position) =
    position
