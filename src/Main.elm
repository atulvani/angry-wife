module Main exposing (..)

import Arsenal exposing (Arsenal, ArsenalSlotType, arsenalSlotType1, arsenalSlotType2, arsenalSlotType3, arsenalSlotType4)
import Browser
import Browser.Events exposing (onKeyDown)
import Game exposing (..)
import Html exposing (Html, div, li, text, ul)
import Html.Attributes exposing (class, style)
import Json.Decode
import Physics exposing (Dimensions, Eta, Position, areEtasSame, decEta, etaToFloat, isEtaZero, tickSpan)
import Player
import PlayerType exposing (PlayerType, getOtherPlayerType, husbandType, wifeType)
import Random
import Random.List
import Stash exposing (StashSlotType, stashSlotType1, stashSlotType2, stashSlotType3, stashSlotType4)
import Time
import Utils exposing (findFromList, ifThenElse, uniqList)
import Weapon exposing (Weapon, getWeaponName, makeWeapon)
import WeaponLocation exposing (WeaponLocation, makeWeaponLocation)


type alias AttackPos =
    { weapon : Weapon
    , victimPlayerType : PlayerType
    , eta : Eta
    , startPosition : Position
    , currentPosition : Position
    , endPosition : Position
    }


type alias Coords =
    { bottom : Float
    , left : Float
    }


type alias Model =
    { game : Game
    , attackPosList : List AttackPos
    }


type Msg
    = NoOp
    | AwardStashOnInit ( List WeaponLocation, List WeaponLocation )
    | GameTicked Time.Posix
    | StashAccessed PlayerType StashSlotType
    | StashAwarded ( WeaponLocation, WeaponLocation )
    | WeaponAccessed PlayerType ArsenalSlotType
    | WeaponAwarded ( Maybe Weapon, Maybe Weapon )


type alias WeaponModel =
    { weapon : Weapon
    , weaponLocations : List WeaponLocation
    }


playerDimensions : Dimensions
playerDimensions =
    { width = 20, height = 160 }


stageDimensions : Dimensions
stageDimensions =
    { width = 1440, height = 500 }


getArmPosition : Position -> Position
getArmPosition ({ y } as p) =
    { p | y = y + (playerDimensions.height * 75 / stageDimensions.height) }


getCoordsFromPosition : Position -> Coords
getCoordsFromPosition { x, y } =
    { bottom = y * stageDimensions.height / 100, left = x * stageDimensions.width / 100 }


getPositionalStyles : Float -> Float -> Position -> List (Html.Attribute msg)
getPositionalStyles h w ({ z } as position) =
    let
        { bottom, left } =
            getCoordsFromPosition position

        horizontalScaleFactor =
            w * ((z - 10) / 90)

        verticalScaleFactor =
            h * ((z - 10) / 90)
    in
    [ style "bottom" (floatToPx (bottom - (verticalScaleFactor / 2)))
    , style "height" (floatToPx (h + verticalScaleFactor))
    , style "left" (floatToPx (left - (horizontalScaleFactor / 2)))
    , style "width" (floatToPx (w + horizontalScaleFactor))
    , style "z-index" (String.fromFloat z)
    ]


getPlayerPosition : PlayerType -> Game -> Position
getPlayerPosition playerType game =
    game |> getPlayerByType playerType |> Player.getPosition


getPlayerArsenalByType : PlayerType -> Game -> Arsenal
getPlayerArsenalByType playerType game =
    game |> getPlayerByType playerType |> Player.getArsenal


updateModel : Msg -> Model -> ( Model, Cmd Msg )
updateModel message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        AwardStashOnInit ( hWeaponLocations, wWeaponLocations ) ->
            ( { model | game = initStashForPlayers ( hWeaponLocations, wWeaponLocations ) model.game }, Cmd.none )

        GameTicked _ ->
            let
                ( game, stashSlotTypePair ) =
                    updateGame model.game

                ( newGame, maybeWeaponAwardedCmd ) =
                    accessStashForPlayers WeaponAwarded stashSlotTypePair game

                doesAttackMatch =
                    \{ weapon, victimPlayerType, eta } ( w, vpt, e ) ->
                        weapon == w && victimPlayerType == vpt && areEtasSame e eta

                updatedAttackPosList =
                    model.attackPosList
                        |> List.map (\({ eta } as attackPos) -> { attackPos | eta = decEta eta })
                        |> List.filter (\{ eta } -> not (isEtaZero eta))
                        |> List.map
                            (\({ victimPlayerType } as attackPos) ->
                                { attackPos | endPosition = getArmPosition (getPlayerPosition victimPlayerType game) }
                            )
                        |> List.map
                            (\({ startPosition, currentPosition, endPosition, eta } as attackPos) ->
                                let
                                    distanceTravelled =
                                        abs (startPosition.x - currentPosition.x)

                                    distanceRemaining =
                                        currentPosition.x - endPosition.x

                                    xDiff =
                                        (distanceRemaining / etaToFloat eta) * tickSpan

                                    yDiff =
                                        (abs distanceTravelled - abs distanceRemaining) * 0.01

                                    zDiff =
                                        ((currentPosition.z - endPosition.z) / etaToFloat eta) * tickSpan
                                in
                                { attackPos
                                    | currentPosition =
                                        { currentPosition
                                            | x = currentPosition.x - xDiff
                                            , y = currentPosition.y - yDiff
                                            , z = currentPosition.z - zDiff
                                        }
                                }
                            )

                newAttackPosList =
                    updatedAttackPosList
                        -- add new onGoingAttacks to the attackPosList
                        ++ (newGame
                                |> getOnGoingAttacks
                                |> List.filter
                                    (\onGoingAttackTuple ->
                                        List.all (\attackPos -> not (doesAttackMatch attackPos onGoingAttackTuple)) updatedAttackPosList
                                    )
                                |> List.map
                                    (\( weapon, vpt, eta ) ->
                                        { weapon = weapon
                                        , victimPlayerType = vpt
                                        , eta = eta
                                        , startPosition = getArmPosition (getPlayerPosition (getOtherPlayerType vpt) newGame)
                                        , currentPosition = getArmPosition (getPlayerPosition (getOtherPlayerType vpt) newGame)
                                        , endPosition = getArmPosition (getPlayerPosition vpt newGame)
                                        }
                                    )
                           )
            in
            ( { model
                | game = newGame
                , attackPosList = newAttackPosList
              }
            , Maybe.withDefault Cmd.none maybeWeaponAwardedCmd
            )

        StashAccessed playerType slotType ->
            ( { model | game = movePlayerToStash playerType slotType model.game }, Cmd.none )

        StashAwarded weaponLocationPair ->
            ( { model | game = awardStashToPlayers weaponLocationPair model.game }, Cmd.none )

        WeaponAccessed attackerPlayerType slotType ->
            ( { model | game = usePlayerWeapon attackerPlayerType slotType model.game }, Cmd.none )

        WeaponAwarded weaponPair ->
            let
                ( game, maybeStashAwardedCmd ) =
                    awardWeaponToPlayers StashAwarded weaponPair model.game
            in
            ( { model | game = game }, Maybe.withDefault Cmd.none maybeStashAwardedCmd )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every tickSpan GameTicked
        , onKeyDown
            (Json.Decode.map
                (\k ->
                    case String.toUpper k of
                        "K" ->
                            StashAccessed husbandType stashSlotType1

                        "L" ->
                            StashAccessed husbandType stashSlotType2

                        ":" ->
                            StashAccessed husbandType stashSlotType3

                        ";" ->
                            StashAccessed husbandType stashSlotType3

                        "\"" ->
                            StashAccessed husbandType stashSlotType4

                        "'" ->
                            StashAccessed husbandType stashSlotType4

                        "A" ->
                            StashAccessed wifeType stashSlotType1

                        "S" ->
                            StashAccessed wifeType stashSlotType2

                        "D" ->
                            StashAccessed wifeType stashSlotType3

                        "F" ->
                            StashAccessed wifeType stashSlotType4

                        _ ->
                            NoOp
                )
                (Json.Decode.field "key" Json.Decode.string)
            )
        , onKeyDown
            (Json.Decode.map
                (\k ->
                    case String.toUpper k of
                        "Q" ->
                            WeaponAccessed wifeType arsenalSlotType1

                        "W" ->
                            WeaponAccessed wifeType arsenalSlotType2

                        "E" ->
                            WeaponAccessed wifeType arsenalSlotType3

                        "R" ->
                            WeaponAccessed wifeType arsenalSlotType4

                        "I" ->
                            WeaponAccessed husbandType arsenalSlotType1

                        "O" ->
                            WeaponAccessed husbandType arsenalSlotType2

                        "P" ->
                            WeaponAccessed husbandType arsenalSlotType3

                        "{" ->
                            WeaponAccessed husbandType arsenalSlotType4

                        "[" ->
                            WeaponAccessed husbandType arsenalSlotType4

                        _ ->
                            NoOp
                )
                (Json.Decode.field "key" Json.Decode.string)
            )
        ]


initApp : () -> ( Model, Cmd Msg )
initApp _ =
    let
        husbandWeaponLocations =
            uniqList (List.concatMap .weaponLocations husbandWeaponModelList)

        wifeWeaponLocations =
            uniqList (List.concatMap .weaponLocations wifeWeaponModelList)
    in
    ( { game =
            makeDefaultGame
                playerDimensions
                (List.map (\wm -> ( wm.weapon, wm.weaponLocations )) husbandWeaponModelList)
                (List.map (\wm -> ( wm.weapon, wm.weaponLocations )) wifeWeaponModelList)
      , attackPosList = []
      }
    , Random.generate
        AwardStashOnInit
        (Random.pair (Random.List.shuffle husbandWeaponLocations) (Random.List.shuffle wifeWeaponLocations))
    )


kitchenLowerShelfWeaponLocations : List WeaponLocation
kitchenLowerShelfWeaponLocations =
    [ makeWeaponLocation { x = 0, y = 20, z = 70 }
    , makeWeaponLocation { x = 3, y = 20, z = 60 }
    , makeWeaponLocation { x = 4, y = 28, z = 50 }
    , makeWeaponLocation { x = 7, y = 25, z = 40 }
    , makeWeaponLocation { x = 10, y = 24, z = 30 }
    , makeWeaponLocation { x = 12, y = 29, z = 20 }
    , makeWeaponLocation { x = 13, y = 25, z = 10 }
    , makeWeaponLocation { x = 15, y = 28, z = 10 }
    , makeWeaponLocation { x = 18, y = 26, z = 10 }
    , makeWeaponLocation { x = 20, y = 29, z = 10 }
    , makeWeaponLocation { x = 22, y = 25, z = 10 }
    , makeWeaponLocation { x = 23, y = 30, z = 10 }
    ]


kitchenUpperShelfWeaponLocations : List WeaponLocation
kitchenUpperShelfWeaponLocations =
    [ makeWeaponLocation { x = 1, y = 50, z = 80 }
    , makeWeaponLocation { x = 2, y = 58, z = 70 }
    , makeWeaponLocation { x = 5, y = 48, z = 60 }
    , makeWeaponLocation { x = 6, y = 57, z = 50 }
    , makeWeaponLocation { x = 7.5, y = 56, z = 40 }
    , makeWeaponLocation { x = 8.5, y = 50, z = 30 }
    , makeWeaponLocation { x = 11, y = 53, z = 20 }
    , makeWeaponLocation { x = 13.5, y = 51, z = 10 }
    , makeWeaponLocation { x = 15.5, y = 47, z = 10 }
    , makeWeaponLocation { x = 16, y = 54, z = 10 }
    , makeWeaponLocation { x = 17, y = 47, z = 10 }
    , makeWeaponLocation { x = 19, y = 53, z = 10 }
    , makeWeaponLocation { x = 21, y = 48, z = 10 }
    , makeWeaponLocation { x = 23, y = 55, z = 10 }
    , makeWeaponLocation { x = 23.5, y = 48, z = 10 }
    ]


microwaveWeaponLocations : List WeaponLocation
microwaveWeaponLocations =
    [ makeWeaponLocation { x = 9, y = 38, z = 10 }
    , makeWeaponLocation { x = 12, y = 37, z = 10 }
    ]


refrigeratorWeaponLocations : List WeaponLocation
refrigeratorWeaponLocations =
    [ makeWeaponLocation { x = 26, y = 47, z = 10 }
    , makeWeaponLocation { x = 25, y = 41, z = 10 }
    , makeWeaponLocation { x = 27, y = 35, z = 10 }
    , makeWeaponLocation { x = 25.5, y = 29, z = 10 }
    , makeWeaponLocation { x = 26.5, y = 23, z = 10 }
    ]


washingMachineWeaponLocations : List WeaponLocation
washingMachineWeaponLocations =
    [ makeWeaponLocation { x = 30, y = 29, z = 10 } ]


musicPlayerWeaponLocations : List WeaponLocation
musicPlayerWeaponLocations =
    [ makeWeaponLocation { x = 36, y = 56, z = 10 }
    , makeWeaponLocation { x = 39, y = 54.5, z = 10 }
    ]


cupboardWeaponLocations : List WeaponLocation
cupboardWeaponLocations =
    [ makeWeaponLocation { x = 33.5, y = 48, z = 10 }
    , makeWeaponLocation { x = 39, y = 48.5, z = 10 }
    , makeWeaponLocation { x = 37, y = 47.5, z = 10 }
    , makeWeaponLocation { x = 34, y = 42, z = 10 }
    , makeWeaponLocation { x = 37.5, y = 41, z = 10 }
    , makeWeaponLocation { x = 40, y = 41.5, z = 10 }
    , makeWeaponLocation { x = 34.5, y = 35.5, z = 10 }
    , makeWeaponLocation { x = 39, y = 34, z = 10 }
    , makeWeaponLocation { x = 34, y = 24, z = 10 }
    , makeWeaponLocation { x = 36, y = 28, z = 10 }
    , makeWeaponLocation { x = 39.5, y = 25.5, z = 10 }
    ]


openShelfWeaponLocations : List WeaponLocation
openShelfWeaponLocations =
    [ makeWeaponLocation { x = 42, y = 24.5, z = 10 }
    , makeWeaponLocation { x = 41.5, y = 32, z = 10 }
    , makeWeaponLocation { x = 42.5, y = 39, z = 10 }
    , makeWeaponLocation { x = 42, y = 45, z = 10 }
    , makeWeaponLocation { x = 45, y = 45.5, z = 10 }
    , makeWeaponLocation { x = 48, y = 49, z = 10 }
    , makeWeaponLocation { x = 50, y = 48, z = 10 }
    , makeWeaponLocation { x = 47, y = 27, z = 10 }
    , makeWeaponLocation { x = 48, y = 23, z = 10 }
    , makeWeaponLocation { x = 55, y = 46, z = 10 }
    , makeWeaponLocation { x = 58, y = 46.5, z = 10 }
    , makeWeaponLocation { x = 55.5, y = 41.5, z = 10 }
    , makeWeaponLocation { x = 57.5, y = 41, z = 10 }
    , makeWeaponLocation { x = 56, y = 27, z = 10 }
    , makeWeaponLocation { x = 55, y = 23.5, z = 10 }
    , makeWeaponLocation { x = 63.5, y = 22.5, z = 10 }
    , makeWeaponLocation { x = 64.5, y = 26, z = 10 }
    , makeWeaponLocation { x = 64, y = 30.5, z = 10 }
    ]


tvDrawerWeaponLocations : List WeaponLocation
tvDrawerWeaponLocations =
    [ makeWeaponLocation { x = 45, y = 23, z = 10 }
    , makeWeaponLocation { x = 51, y = 24, z = 10 }
    ]


tvTableWeaponLocations : List WeaponLocation
tvTableWeaponLocations =
    [ makeWeaponLocation { x = 45, y = 29, z = 10 }
    , makeWeaponLocation { x = 50, y = 29, z = 10 }
    ]


storageShelfWeaponLocations : List WeaponLocation
storageShelfWeaponLocations =
    [ makeWeaponLocation { x = 53.5, y = 46, z = 10 }
    , makeWeaponLocation { x = 53, y = 38, z = 10 }
    , makeWeaponLocation { x = 53.5, y = 30, z = 10 }
    , makeWeaponLocation { x = 53, y = 24, z = 10 }
    , makeWeaponLocation { x = 55, y = 36.5, z = 10 }
    , makeWeaponLocation { x = 56, y = 32.5, z = 10 }
    , makeWeaponLocation { x = 58, y = 35, z = 10 }
    , makeWeaponLocation { x = 57.5, y = 29.5, z = 10 }
    , makeWeaponLocation { x = 58.5, y = 24.5, z = 10 }
    ]


pcTableWeaponLocations : List WeaponLocation
pcTableWeaponLocations =
    [ makeWeaponLocation { x = 60, y = 33, z = 10 }
    , makeWeaponLocation { x = 62, y = 32, z = 10 }
    ]


centerTableWeaponLocations : List WeaponLocation
centerTableWeaponLocations =
    [ makeWeaponLocation { x = 74, y = 22, z = 10 }
    , makeWeaponLocation { x = 72.5, y = 25, z = 10 }
    , makeWeaponLocation { x = 76, y = 26, z = 10 }
    , makeWeaponLocation { x = 78, y = 23, z = 10 }
    ]


weaponLocations : List WeaponLocation
weaponLocations =
    kitchenLowerShelfWeaponLocations
        ++ kitchenUpperShelfWeaponLocations
        -- ++ microwaveWeaponLocations
        ++ refrigeratorWeaponLocations
        -- ++ washingMachineWeaponLocations
        ++ musicPlayerWeaponLocations
        ++ cupboardWeaponLocations
        ++ openShelfWeaponLocations
        ++ tvDrawerWeaponLocations
        ++ tvTableWeaponLocations
        ++ storageShelfWeaponLocations
        ++ pcTableWeaponLocations
        ++ centerTableWeaponLocations


unisexWeaponModelList : List WeaponModel
unisexWeaponModelList =
    [ { weapon = makeWeapon "fork.svg" 1 True False, weaponLocations = kitchenLowerShelfWeaponLocations ++ kitchenUpperShelfWeaponLocations }
    , { weapon = makeWeapon "cd.svg" 1 True False, weaponLocations = musicPlayerWeaponLocations ++ pcTableWeaponLocations }
    , { weapon = makeWeapon "smallTeddy.svg" 1 True False, weaponLocations = openShelfWeaponLocations }

    --
    , { weapon = makeWeapon "mouse.svg" 2 True False, weaponLocations = pcTableWeaponLocations }
    , { weapon = makeWeapon "tvRemote1.svg" 2 True False, weaponLocations = tvDrawerWeaponLocations ++ tvTableWeaponLocations }
    , { weapon = makeWeapon "tvRemote2.svg" 2 True False, weaponLocations = tvDrawerWeaponLocations ++ tvTableWeaponLocations }
    , { weapon = makeWeapon "shaver.svg" 2 True False, weaponLocations = cupboardWeaponLocations ++ storageShelfWeaponLocations }
    , { weapon = makeWeapon "musicCassette.svg" 2 True False, weaponLocations = musicPlayerWeaponLocations }
    , { weapon = makeWeapon "bigTeddy.svg" 2 True False, weaponLocations = openShelfWeaponLocations }
    , { weapon = makeWeapon "apple.svg" 2 True False, weaponLocations = refrigeratorWeaponLocations }
    , { weapon = makeWeapon "banana.svg" 2 True False, weaponLocations = refrigeratorWeaponLocations }
    , { weapon = makeWeapon "carrot.svg" 2 True False, weaponLocations = refrigeratorWeaponLocations }
    , { weapon = makeWeapon "beerBottle.svg" 2 True False, weaponLocations = refrigeratorWeaponLocations }
    , { weapon = makeWeapon "sosBottle.svg" 2 True False, weaponLocations = refrigeratorWeaponLocations }
    , { weapon = makeWeapon "hourGlass.svg" 2 True False, weaponLocations = refrigeratorWeaponLocations }
    , { weapon = makeWeapon "scissor.svg" 2 True False, weaponLocations = refrigeratorWeaponLocations }

    --
    , { weapon = makeWeapon "videoCassette.svg" 3 True False, weaponLocations = tvDrawerWeaponLocations ++ tvTableWeaponLocations }
    , { weapon = makeWeapon "gamingConsoleRemote.svg" 3 True False, weaponLocations = tvDrawerWeaponLocations ++ tvTableWeaponLocations }
    , { weapon = makeWeapon "mug.svg" 3 True False, weaponLocations = centerTableWeaponLocations }
    , { weapon = makeWeapon "sodaCan.svg" 3 True False, weaponLocations = refrigeratorWeaponLocations }
    , { weapon = makeWeapon "cake.svg" 3 True False, weaponLocations = refrigeratorWeaponLocations }

    --
    , { weapon = makeWeapon "coconut.svg" 4 True False, weaponLocations = refrigeratorWeaponLocations }
    , { weapon = makeWeapon "wineBottle.svg" 4 True False, weaponLocations = openShelfWeaponLocations }
    , { weapon = makeWeapon "champagneBottle.svg" 4 True False, weaponLocations = openShelfWeaponLocations }

    --
    , { weapon = makeWeapon "iron.svg" 5 True False, weaponLocations = cupboardWeaponLocations ++ storageShelfWeaponLocations }
    , { weapon = makeWeapon "vase.svg" 5 True False, weaponLocations = openShelfWeaponLocations }
    , { weapon = makeWeapon "flowerPot.svg" 5 True False, weaponLocations = openShelfWeaponLocations }
    ]


husbandWeaponModelList : List WeaponModel
husbandWeaponModelList =
    [ { weapon = makeWeapon "football.svg" 3 True False, weaponLocations = cupboardWeaponLocations ++ storageShelfWeaponLocations }
    , { weapon = makeWeapon "rugbyball.svg" 3 True False, weaponLocations = cupboardWeaponLocations ++ storageShelfWeaponLocations }
    , { weapon = makeWeapon "manShoe.svg" 4 True False, weaponLocations = cupboardWeaponLocations ++ storageShelfWeaponLocations }
    , { weapon = makeWeapon "boxingGlove.svg" 4 True False, weaponLocations = openShelfWeaponLocations }
    , { weapon = makeWeapon "basketball.svg" 5 True False, weaponLocations = cupboardWeaponLocations ++ storageShelfWeaponLocations }
    ]
        ++ unisexWeaponModelList


wifeWeaponModelList : List WeaponModel
wifeWeaponModelList =
    [ { weapon = makeWeapon "dish.svg" 3 True False, weaponLocations = kitchenLowerShelfWeaponLocations ++ kitchenUpperShelfWeaponLocations }
    , { weapon = makeWeapon "pan.svg" 3 True False, weaponLocations = kitchenLowerShelfWeaponLocations ++ kitchenUpperShelfWeaponLocations }
    , { weapon = makeWeapon "womanShoe.svg" 4 True False, weaponLocations = cupboardWeaponLocations ++ storageShelfWeaponLocations }
    , { weapon = makeWeapon "bowl.svg" 4 True False, weaponLocations = kitchenLowerShelfWeaponLocations ++ kitchenUpperShelfWeaponLocations }
    , { weapon = makeWeapon "kettle.svg" 4 True False, weaponLocations = kitchenLowerShelfWeaponLocations ++ kitchenUpperShelfWeaponLocations }
    ]
        ++ unisexWeaponModelList


allWeaponModelList : List WeaponModel
allWeaponModelList =
    husbandWeaponModelList ++ wifeWeaponModelList


floatToPx : Float -> String
floatToPx num =
    String.fromFloat num ++ "px"


getStageView : Model -> List (Html Msg) -> Html Msg
getStageView { game } nestedView =
    let
        ( husbandHealth, wifeHealth ) =
            getPlayerHealthAsTuple game

        hasGameFinished =
            (husbandHealth == 0) || (wifeHealth == 0)

        gameStateAttributes =
            ifThenElse hasGameFinished [ style "pointer-events" "none" ] []

        gameStateView =
            ifThenElse
                hasGameFinished
                [ div
                    [ style "background" "#000", style "height" "100%", style "opacity" "0.5", style "width" "100%", style "z-index" "999" ]
                    []
                ]
                []
    in
    div
        ([ class "studio"
         , style "height" (floatToPx stageDimensions.height)
         , style "width" (floatToPx stageDimensions.width)
         ]
            ++ gameStateAttributes
        )
        (nestedView ++ gameStateView)


getPlayerView : PlayerType -> Model -> Html Msg
getPlayerView playerType { game } =
    let
        color =
            ifThenElse (playerType == husbandType) "#99f" "#f9f"

        position =
            getPlayerPosition playerType game
    in
    div
        ([ class "player", style "background" color ]
            ++ getPositionalStyles playerDimensions.height playerDimensions.width position
        )
        []


getWeaponLocationsDebugView : Model -> Html Msg
getWeaponLocationsDebugView _ =
    -- to be used when setting up weapon locations for a new scene
    div
        []
        (weaponLocations
            |> List.map (WeaponLocation.toPosition >> getCoordsFromPosition)
            |> List.map
                (\{ bottom, left } ->
                    div
                        [ style "background" "yellow"
                        , style "border" "3px solid orange"
                        , style "border-radius" "50%"
                        , style "bottom" (floatToPx bottom)
                        , style "height" "20px"
                        , style "left" (floatToPx left)
                        , style "position" "absolute"
                        , style "width" "20px"
                        ]
                        []
                )
        )


getWeaponLocationsView : Model -> Html Msg
getWeaponLocationsView { game } =
    div [] (List.map getWeaponLocationView (getActiveStashes game))


getWeaponLocationView : ( PlayerType, StashSlotType, Position ) -> Html Msg
getWeaponLocationView ( pt, st, position ) =
    let
        { bottom, left } =
            getCoordsFromPosition position

        bgColor =
            ifThenElse (pt == husbandType) "#99c" "#c99"

        borderColor =
            ifThenElse (pt == husbandType) "#336" "#633"

        shortcut =
            if pt == husbandType && st == stashSlotType1 then
                "K"

            else if pt == husbandType && st == stashSlotType2 then
                "L"

            else if pt == husbandType && st == stashSlotType3 then
                ":"

            else if pt == husbandType && st == stashSlotType4 then
                "\""

            else if pt == wifeType && st == stashSlotType1 then
                "A"

            else if pt == wifeType && st == stashSlotType2 then
                "S"

            else if pt == wifeType && st == stashSlotType3 then
                "D"

            else
                "F"
    in
    div
        [ class "weapon-location"
        , style "background" bgColor
        , style "border-color" borderColor
        , style "bottom" (floatToPx bottom)
        , style "left" (floatToPx left)
        ]
        [ text shortcut ]


getArsenalView : Model -> Html Msg
getArsenalView { game } =
    let
        render =
            \pt st key ->
                let
                    maybeWeapon =
                        Arsenal.getWeaponBySlotType st (getPlayerArsenalByType pt game)

                    maybeWeaponModel =
                        case maybeWeapon of
                            Just w ->
                                findFromList (\{ weapon } -> weapon == w) allWeaponModelList

                            _ ->
                                Nothing

                    clickableAttrs =
                        case Maybe.map (.weapon >> getWeaponName) maybeWeaponModel of
                            Nothing ->
                                [ style "border" "3px solid #666", style "color" "#666" ]

                            Just name ->
                                [ style "background" ("top center /contain no-repeat url(./images/" ++ name ++ ")")
                                , style "color" "#fff"
                                ]

                    attrs =
                        clickableAttrs
                            ++ [ style "margin" "60px 30px"
                               , style "padding-top" "25px"
                               , style "box-sizing" "border-box"
                               , style "border-radius" "50%"
                               , style "width" "30px"
                               , style "height" "30px"
                               , style "line-height" "30px"
                               , style "text-align" "center"
                               ]
                in
                li attrs [ text (ifThenElse (maybeWeapon == Nothing) "" key) ]
    in
    div
        [ class "arsenal" ]
        [ ul
            [ class "wife" ]
            [ render wifeType arsenalSlotType1 "Q"
            , render wifeType arsenalSlotType2 "W"
            , render wifeType arsenalSlotType3 "E"
            , render wifeType arsenalSlotType4 "R"
            ]
        , ul
            [ class "husband" ]
            [ render husbandType arsenalSlotType1 "I"
            , render husbandType arsenalSlotType2 "O"
            , render husbandType arsenalSlotType3 "P"
            , render husbandType arsenalSlotType4 "{"
            ]
        ]


getPlayerHealthView : Model -> Html Msg
getPlayerHealthView { game } =
    let
        ( husbandHealth, wifeHealth ) =
            getPlayerHealthAsTuple game
    in
    div
        [ class "health" ]
        [ div
            [ class "wife", style "background" "#636" ]
            [ div [ style "background" "#f9f", style "width" (String.fromFloat wifeHealth ++ "%") ] [] ]
        , div
            [ class "husband", style "background" "#336" ]
            [ div [ style "background" "#99f", style "width" (String.fromFloat husbandHealth ++ "%") ] [] ]
        ]


getAttacksView : Model -> Html Msg
getAttacksView { attackPosList } =
    let
        getWeaponImg =
            \weapon ->
                allWeaponModelList
                    |> findFromList (.weapon >> (==) weapon)
                    |> Maybe.map (.weapon >> getWeaponName)
                    -- the below should never happen
                    |> Maybe.withDefault ""
    in
    div
        [ class "attacks" ]
        (attackPosList
            |> List.map
                (\{ currentPosition, weapon } ->
                    div
                        ([ class "attack"
                         , style "background" ("top center /contain no-repeat url(./images/" ++ getWeaponImg weapon ++ ")")
                         ]
                            ++ getPositionalStyles 30 30 currentPosition
                        )
                        []
                )
        )


view : Model -> Html Msg
view model =
    div
        [ class "wrapper" ]
        [ getStageView
            model
            [ getPlayerView husbandType model
            , getPlayerView wifeType model

            -- , getWeaponLocationsDebugView model
            , getWeaponLocationsView model
            , getArsenalView model
            , getPlayerHealthView model
            , getAttacksView model
            ]
        ]


main : Program () Model Msg
main =
    Browser.element { init = initApp, update = updateModel, subscriptions = subscriptions, view = view }
