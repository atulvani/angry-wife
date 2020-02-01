module Utils exposing (complement, findFromList, flattenMaybe, flip, ifThenElse, mapCaseOf, uniqList)

import Tuple


complement : (a -> Bool) -> (a -> Bool)
complement predicate =
    \a -> not (predicate a)


findFromList : (a -> Bool) -> List a -> Maybe a
findFromList predicate list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            ifThenElse (predicate x) (Just x) (findFromList predicate xs)


flattenMaybe : Maybe (Maybe a) -> Maybe a
flattenMaybe a =
    case a of
        Just b ->
            b

        _ ->
            Nothing


flip : (a -> b -> c) -> (b -> a -> c)
flip func =
    \a b -> func b a


ifThenElse : Bool -> arg -> arg -> arg
ifThenElse shouldReturnFirst arg1 arg2 =
    if shouldReturnFirst then
        arg1

    else
        arg2


mapCaseOf : (a -> Bool) -> List ( a, b ) -> Maybe b
mapCaseOf predicate caseList =
    caseList |> findFromList (\( c, _ ) -> predicate c) |> Maybe.map Tuple.second


uniqList : List a -> List a
uniqList list =
    List.foldl (\item acc -> ifThenElse (List.member item acc) acc (item :: acc)) [] list
