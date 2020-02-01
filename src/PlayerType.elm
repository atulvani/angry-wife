module PlayerType exposing (PlayerType(..), getOtherPlayerType, husbandType, wifeType)

import Utils exposing (ifThenElse)


type PlayerType
    = PlayerType String


husbandType : PlayerType
husbandType =
    PlayerType "husband"


wifeType : PlayerType
wifeType =
    PlayerType "wife"


getOtherPlayerType : PlayerType -> PlayerType
getOtherPlayerType pt =
    ifThenElse (pt == wifeType) husbandType wifeType
