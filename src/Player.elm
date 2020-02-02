module Player exposing (Energy, Health, Player(..), Target(..), accessStash, awardStash, awardWeapon, getArsenal, getHealthAsFloat, getPosition, getStashAsTuple, initStash, makeDefaultPlayer, moveToStash, reduceHealth, updatePlayer, useWeapon)

import Arsenal exposing (Arsenal, ArsenalSlotType, accessArsenal, addWeaponToArsenal, isArsenalFull, makeEmptyArsenal)
import Physics exposing (Dimensions, Eta, Position, Speed, adjustPosition, calcEta, calcHorizDistance, decEta, isEtaZero, playerSpeed)
import Stash exposing (Stash, StashSlotType, addToStash, makeEmptyStash, populateStashFromList, stashSlotType1, stashSlotType2, stashSlotType3, stashSlotType4)
import Utils exposing (ifThenElse)
import Weapon exposing (Weapon)
import WeaponLocation exposing (WeaponLocation)


type alias Player_ =
    { dimensions : Dimensions
    , position : Position
    , target : Maybe Target
    , energy : Energy
    , health : Health
    , isAngry : Bool
    , arsenal : Arsenal
    , stash : Stash
    }


type Player
    = Player Player_


type alias Target_ =
    { position : Position
    , eta : Eta
    }


type Target
    = Target Target_


type Energy
    = Energy Float


type Health
    = Health Float


accessStash : Player -> StashSlotType -> ( Player, Maybe WeaponLocation )
accessStash (Player player) slotType =
    case Stash.access slotType player.stash of
        ( Just weaponLocation, stash ) ->
            ( Player { player | stash = stash }, Just weaponLocation )

        _ ->
            ( Player player, Nothing )


awardStash : Player -> WeaponLocation -> Player
awardStash (Player player) weaponLocation =
    Player { player | stash = addToStash weaponLocation player.stash }


awardWeapon : Player -> Weapon -> Player
awardWeapon (Player player) weapon =
    Player { player | arsenal = addWeaponToArsenal weapon player.arsenal }


decTargetEta : Target -> Target
decTargetEta (Target target) =
    Target { target | eta = decEta target.eta }


getHealthAsFloat : Player -> Float
getHealthAsFloat (Player { health }) =
    let
        (Health healthAsFloat) =
            health
    in
    healthAsFloat


getPosition : Player -> Position
getPosition (Player { position }) =
    position


getStashAsTuple : Player -> List ( StashSlotType, Position )
getStashAsTuple (Player player) =
    List.filterMap
        (\( stashSlotType, maybePosition ) ->
            case maybePosition of
                Just position ->
                    Just ( stashSlotType, position )

                _ ->
                    Nothing
        )
        [ ( stashSlotType1, Stash.getPosition stashSlotType1 player.stash )
        , ( stashSlotType2, Stash.getPosition stashSlotType2 player.stash )
        , ( stashSlotType3, Stash.getPosition stashSlotType3 player.stash )
        , ( stashSlotType4, Stash.getPosition stashSlotType4 player.stash )
        ]


getArsenal : Player -> Arsenal
getArsenal (Player { arsenal }) =
    arsenal


initStash : List WeaponLocation -> Player -> Player
initStash weaponLocations (Player player) =
    Player { player | stash = populateStashFromList weaponLocations player.stash }


makeDefaultPlayer : Dimensions -> Position -> Player
makeDefaultPlayer dimensions position =
    makePlayer dimensions position Nothing (Energy 100) (Health 100) False (makeEmptyArsenal ()) (makeEmptyStash ())


makePlayer : Dimensions -> Position -> Maybe Target -> Energy -> Health -> Bool -> Arsenal -> Stash -> Player
makePlayer dimensions position target energy health isAngry arsenal stash =
    Player
        { dimensions = dimensions
        , position = position
        , target = target
        , energy = energy
        , health = health
        , isAngry = isAngry
        , arsenal = arsenal
        , stash = stash
        }


moveToStash : StashSlotType -> Player -> Player
moveToStash slotType (Player player) =
    case Stash.getPosition slotType player.stash of
        Nothing ->
            Player player

        Just position ->
            setTarget playerSpeed position player


reduceHealth : Player -> Player
reduceHealth (Player player) =
    Player { player | health = Health (getHealthAsFloat (Player player) - 10) }


setTarget : Speed -> Position -> Player_ -> Player
setTarget speed position player =
    let
        targetPosition =
            { position | y = player.position.y }

        eta =
            calcEta speed (calcHorizDistance player.position targetPosition)

        target =
            Just (Target { position = targetPosition, eta = eta })
    in
    Player { player | target = target }


updatePlayer : Player -> ( Player, Maybe StashSlotType )
updatePlayer ((Player { position, stash, arsenal }) as player) =
    let
        ((Player { target }) as newPlayer) =
            updatePosition player

        maybeStopPosition =
            ifThenElse (target == Nothing) (Just position) Nothing

        maybeStopSlotType =
            maybeStopPosition |> Maybe.andThen (Stash.getSlotTypeByPosition stash)
    in
    ( newPlayer, ifThenElse (isArsenalFull arsenal) Nothing maybeStopSlotType )


updatePosition : Player -> Player
updatePosition (Player ({ position, target } as player)) =
    target
        |> Maybe.map decTargetEta
        |> Maybe.map
            (\(Target newTarget) ->
                Player
                    { player
                        | position = adjustPosition position newTarget.position newTarget.eta
                        , target = ifThenElse (isEtaZero newTarget.eta) Nothing (Just (Target newTarget))
                    }
            )
        |> Maybe.withDefault (Player player)


useWeapon : ArsenalSlotType -> Player -> Maybe ( Player, Weapon )
useWeapon slotType (Player player) =
    case accessArsenal slotType player.arsenal of
        ( Just weapon, arsenal ) ->
            Just ( Player { player | arsenal = arsenal }, weapon )

        _ ->
            Nothing
