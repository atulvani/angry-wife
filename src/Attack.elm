module Attack exposing (Attack(..), getAttackAsTuple, isAttackFinished, isOnPlayerType, makeAttack, updateAttack)

import Physics exposing (Eta, decEta, isEtaZero)
import PlayerType exposing (PlayerType)
import Weapon exposing (Weapon)


type alias Attack_ =
    { weapon : Weapon
    , victimPlayerType : PlayerType
    , eta : Eta
    }


type Attack
    = Attack Attack_


type alias AttackTuple =
    ( Weapon, PlayerType, Eta )


getAttackAsTuple : Attack -> AttackTuple
getAttackAsTuple (Attack { weapon, victimPlayerType, eta }) =
    ( weapon, victimPlayerType, eta )


isAttackFinished : Attack -> Bool
isAttackFinished (Attack attack) =
    isEtaZero attack.eta


isOnPlayerType : PlayerType -> Attack -> Bool
isOnPlayerType pt (Attack { victimPlayerType }) =
    pt == victimPlayerType


makeAttack : Weapon -> PlayerType -> Eta -> Attack
makeAttack weapon victimPlayerType eta =
    Attack { weapon = weapon, victimPlayerType = victimPlayerType, eta = eta }


updateAttack : Attack -> Attack
updateAttack (Attack attack) =
    Attack { attack | eta = decEta attack.eta }
