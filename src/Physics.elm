module Physics exposing (Dimensions, Distance(..), Eta(..), Position, Speed(..), adjustPosition, areEtasSame, arePositionsHorizontallyEquivalent, calcEta, calcHorizDistance, decEta, etaToFloat, isEtaZero, playerSpeed, tickSpan, weaponSpeed)


type alias Dimensions =
    { width : Float
    , height : Float
    }


type Distance
    = Distance Float


type Eta
    = Eta Float


type alias Position =
    { x : Float
    , y : Float
    , z : Float
    }


type Speed
    = Speed Float


adjustPosition : Position -> Position -> Eta -> Position
adjustPosition currentPosition targetPosition (Eta eta) =
    if eta <= 0 then
        targetPosition

    else
        let
            fractionTravelled =
                Basics.min 1 (tickSpan / eta)
        in
        { x = currentPosition.x + ((targetPosition.x - currentPosition.x) * fractionTravelled)
        , y = currentPosition.y + ((targetPosition.y - currentPosition.y) * fractionTravelled)
        , z = currentPosition.z + ((targetPosition.z - currentPosition.z) * fractionTravelled)
        }


areEtasSame : Eta -> Eta -> Bool
areEtasSame (Eta e1) (Eta e2) =
    e1 == e2


arePositionsHorizontallyEquivalent : Position -> Position -> Bool
arePositionsHorizontallyEquivalent p1 p2 =
    p1.x == p2.x && p1.z == p2.z


calcHorizDistance : Position -> Position -> Distance
calcHorizDistance p1 p2 =
    Distance (sqrt (((p1.x - p2.x) * (p1.x - p2.x)) + (((p1.z - p2.z) * (p1.z - p2.z)) / 2)))


calcEta : Speed -> Distance -> Eta
calcEta (Speed speed) (Distance distance) =
    Eta (distance / speed)


decEta : Eta -> Eta
decEta (Eta eta) =
    Eta (Basics.max 0 (eta - tickSpan))


isEtaZero : Eta -> Bool
isEtaZero (Eta eta) =
    eta == 0


tickSpan : Float
tickSpan =
    10


etaToFloat : Eta -> Float
etaToFloat (Eta eta) =
    eta


playerSpeed : Speed
playerSpeed =
    Speed 0.01


weaponSpeed : Speed
weaponSpeed =
    Speed 0.05
