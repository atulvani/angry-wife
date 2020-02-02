module Weapon exposing (Weapon(..), getWeaponName, makeWeapon)


type Weapon
    = Weapon
        { weight : Float
        , isHard : Bool
        , isSharp : Bool

        -- elm appears to miss the equality check withput a unique name field
        , name : String
        }


getWeaponName : Weapon -> String
getWeaponName (Weapon { name }) =
    name


makeWeapon : String -> Float -> Bool -> Bool -> Weapon
makeWeapon name weight isHard isSharp =
    Weapon { name = name, weight = weight, isHard = isHard, isSharp = isSharp }
