module Game exposing (Game(..), accessStashForPlayers, awardStashToPlayers, awardWeaponToPlayers, getActiveStashes, getOnGoingAttacks, getPlayerByType, getPlayerHealthAsTuple, initStashForPlayers, makeDefaultGame, movePlayerToStash, updateGame, usePlayerWeapon)

import Arsenal exposing (ArsenalSlotType)
import Attack exposing (Attack, getAttackAsTuple, makeAttack, updateAttack)
import Physics exposing (Dimensions, Eta, Position, calcEta, calcHorizDistance, weaponSpeed)
import Player exposing (Player, accessStash, awardStash, awardWeapon, getStashAsTuple, initStash, makeDefaultPlayer, moveToStash, updatePlayer, useWeapon)
import PlayerType exposing (PlayerType, husbandType, wifeType)
import Random
import Stash exposing (StashSlotType)
import Tuple
import Utils exposing (ifThenElse, uniqList)
import Weapon exposing (Weapon)
import WeaponLocation exposing (WeaponLocation)


type alias Game_ =
    { husband : Player
    , wife : Player
    , husbandWeapons : List ( Weapon, List WeaponLocation )
    , husbandWeaponLocations : List WeaponLocation
    , wifeWeapons : List ( Weapon, List WeaponLocation )
    , wifeWeaponLocations : List WeaponLocation
    , onGoingAttacks : List Attack
    }


type Game
    = Game Game_


accessStashForPlayers : (( Maybe Weapon, Maybe Weapon ) -> msg) -> ( Maybe StashSlotType, Maybe StashSlotType ) -> Game -> ( Game, Maybe (Cmd msg) )
accessStashForPlayers awardWeaponCmd ( hStashSlotType, wStashSlotType ) (Game ({ husband, wife, husbandWeapons, wifeWeapons } as game)) =
    let
        ( newHusband, maybeHusbandWeaponLocation ) =
            Maybe.withDefault ( husband, Nothing ) (Maybe.map (accessStash husband) hStashSlotType)

        ( newWife, maybeWifeWeaponLocation ) =
            Maybe.withDefault ( wife, Nothing ) (Maybe.map (accessStash wife) wStashSlotType)

        filterWeaponsByWeaponLocation =
            \weapons weaponLocation ->
                List.foldl
                    (\( w, wls ) acc ->
                        ifThenElse (List.member weaponLocation wls) (w :: acc) acc
                    )
                    []
                    weapons

        hWeapons =
            maybeHusbandWeaponLocation
                |> Maybe.map (filterWeaponsByWeaponLocation husbandWeapons)
                |> Maybe.withDefault (List.map Tuple.first husbandWeapons)

        wWeapons =
            maybeWifeWeaponLocation
                |> Maybe.map (filterWeaponsByWeaponLocation wifeWeapons)
                |> Maybe.withDefault (List.map Tuple.first wifeWeapons)
    in
    case ( hWeapons, wWeapons ) of
        ( hw :: hws, ww :: wws ) ->
            let
                weaponsToAwardGenerator =
                    Random.pair (Random.uniform hw hws) (Random.uniform ww wws)

                maybeWeaponsToAwardGenerator =
                    Random.map
                        (\( hWeapon, wWeapon ) ->
                            ( ifThenElse (maybeHusbandWeaponLocation /= Nothing) (Just hWeapon) Nothing
                            , ifThenElse (maybeWifeWeaponLocation /= Nothing) (Just wWeapon) Nothing
                            )
                        )
                        weaponsToAwardGenerator
            in
            ( Game { game | husband = newHusband, wife = newWife }
            , Just (Random.generate awardWeaponCmd maybeWeaponsToAwardGenerator)
            )

        _ ->
            ( Game game, Nothing )


awardStashToPlayers : ( WeaponLocation, WeaponLocation ) -> Game -> Game
awardStashToPlayers ( hWeaponLocation, wWeaponLocation ) (Game game) =
    Game
        { game
            | husband = awardStash game.husband hWeaponLocation
            , wife = awardStash game.wife wWeaponLocation
        }


awardWeaponToPlayers : (( WeaponLocation, WeaponLocation ) -> msg) -> ( Maybe Weapon, Maybe Weapon ) -> Game -> ( Game, Maybe (Cmd msg) )
awardWeaponToPlayers awardStashCmd ( hWeapon, wWeapon ) (Game ({ husband, wife } as game)) =
    case ( game.husbandWeaponLocations, game.wifeWeaponLocations ) of
        ( hwl :: hwls, wwl :: wwls ) ->
            let
                newHusband =
                    ifThenElse (hWeapon == Nothing) husband (Maybe.withDefault husband (Maybe.map (awardWeapon husband) hWeapon))

                newWife =
                    ifThenElse (wWeapon == Nothing) wife (Maybe.withDefault wife (Maybe.map (awardWeapon wife) wWeapon))

                maybeWeaponLocationPair =
                    Random.pair (Random.uniform hwl hwls) (Random.uniform wwl wwls)
            in
            ( Game { game | husband = newHusband, wife = newWife }, Just (Random.generate awardStashCmd maybeWeaponLocationPair) )

        _ ->
            ( Game game, Nothing )


getActiveStashes : Game -> List ( PlayerType, StashSlotType, Position )
getActiveStashes (Game game) =
    List.map (\( st, p ) -> ( husbandType, st, p )) (getStashAsTuple game.husband)
        ++ List.map (\( st, p ) -> ( wifeType, st, p )) (getStashAsTuple game.wife)


getOnGoingAttacks : Game -> List ( Weapon, PlayerType, Eta )
getOnGoingAttacks (Game { onGoingAttacks }) =
    onGoingAttacks
        |> List.map getAttackAsTuple


getPlayerByType : PlayerType -> Game -> Player
getPlayerByType playerType (Game { husband, wife }) =
    ifThenElse (playerType == wifeType) wife husband


getPlayerHealthAsTuple : Game -> ( Float, Float )
getPlayerHealthAsTuple (Game { husband, wife }) =
    ( Player.getHealthAsFloat husband, Player.getHealthAsFloat wife )


initStashForPlayers : ( List WeaponLocation, List WeaponLocation ) -> Game -> Game
initStashForPlayers ( husbandWeaponLocations, wifeWeaponLocations ) (Game game) =
    Game
        { game
            | husband = initStash husbandWeaponLocations game.husband
            , wife = initStash wifeWeaponLocations game.wife
        }


makeDefaultGame : Dimensions -> List ( Weapon, List WeaponLocation ) -> List ( Weapon, List WeaponLocation ) -> Game
makeDefaultGame playerDimensions hws wws =
    makeGame
        playerDimensions
        ( { x = 65, y = 20, z = 40 }, hws )
        ( { x = 20, y = 20, z = 40 }, wws )
        []


makeGame : Dimensions -> ( Position, List ( Weapon, List WeaponLocation ) ) -> ( Position, List ( Weapon, List WeaponLocation ) ) -> List Attack -> Game
makeGame playerDimensions ( hPos, hws ) ( wPos, wws ) onGoingAttacks =
    -- this will more useful when save and continue functionality is implemented
    let
        husbandWeaponLocations =
            uniqList (List.concatMap Tuple.second hws)

        wifeWeaponLocations =
            uniqList (List.concatMap Tuple.second wws)
    in
    Game
        { husband = makeDefaultPlayer playerDimensions hPos
        , wife = makeDefaultPlayer playerDimensions wPos
        , husbandWeapons = hws
        , husbandWeaponLocations = husbandWeaponLocations
        , wifeWeapons = wws
        , wifeWeaponLocations = wifeWeaponLocations
        , onGoingAttacks = onGoingAttacks
        }


movePlayerToStash : PlayerType -> StashSlotType -> Game -> Game
movePlayerToStash playerType slotType (Game game) =
    if playerType == husbandType then
        Game { game | husband = moveToStash slotType game.husband }

    else
        Game { game | wife = moveToStash slotType game.wife }


updateGame : Game -> ( Game, ( Maybe StashSlotType, Maybe StashSlotType ) )
updateGame (Game ({ husband, wife } as game)) =
    let
        ( newHusband, hStashSlotType ) =
            updatePlayer husband

        ( newWife, wStashSlotType ) =
            updatePlayer wife

        newGame =
            updateOnGoingAttacks { game | husband = newHusband, wife = newWife }
    in
    ( Game newGame, ( hStashSlotType, wStashSlotType ) )


updateOnGoingAttacks : Game_ -> Game_
updateOnGoingAttacks ({ husband, wife, onGoingAttacks } as game) =
    let
        ( finishedAttacks, newOnGoingAttacks ) =
            onGoingAttacks |> List.map Attack.updateAttack |> List.partition Attack.isAttackFinished

        newHusband =
            finishedAttacks
                |> List.filter (Attack.isOnPlayerType husbandType)
                -- TODO: introduce hitProbability when making it online to make operation transformation easy
                |> List.foldl (\_ p -> Player.reduceHealth p) husband

        newWife =
            finishedAttacks
                |> List.filter (Attack.isOnPlayerType wifeType)
                -- TODO: make health loss depend on weapon weight, hardness and sharpness
                |> List.foldl (\_ p -> Player.reduceHealth p) wife
    in
    { game | husband = newHusband, wife = newWife, onGoingAttacks = newOnGoingAttacks }


usePlayerWeapon : PlayerType -> ArsenalSlotType -> Game -> Game
usePlayerWeapon attackerPlayerType slotType (Game game) =
    Game game
        |> getPlayerByType attackerPlayerType
        |> useWeapon slotType
        |> Maybe.map
            (\( attackerPlayer, weapon ) ->
                let
                    victimPlayerType =
                        ifThenElse (attackerPlayerType == wifeType) husbandType wifeType

                    newAttack =
                        Game game
                            |> getPlayerByType victimPlayerType
                            |> Player.getPosition
                            |> calcHorizDistance (Player.getPosition attackerPlayer)
                            |> calcEta weaponSpeed
                            |> makeAttack weapon victimPlayerType

                    newOnGoingAttacks =
                        newAttack :: game.onGoingAttacks
                in
                if attackerPlayerType == wifeType then
                    Game { game | wife = attackerPlayer, onGoingAttacks = newOnGoingAttacks }

                else
                    Game { game | husband = attackerPlayer, onGoingAttacks = newOnGoingAttacks }
            )
        |> Maybe.withDefault (Game game)
