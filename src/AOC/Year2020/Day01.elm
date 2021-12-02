module AOC.Year2020.Day01 exposing (part1, part2)

import ListExt
import OrderedDict exposing (OrderedDict)
import Set exposing (Set)


part1 : List String -> String
part1 input =
    input
        |> ListExt.toIntList
        |> findTwo Set.empty 2020
        |> String.fromInt


part2 : List String -> String
part2 input =
    input
        |> ListExt.toIntList
        |> List.sort
        |> List.indexedMap Tuple.pair
        |> OrderedDict.fromList
        |> findThree 0 1 (List.length input - 1) 2020
        |> String.fromInt


findTwo : Set Int -> Int -> List Int -> Int
findTwo set target ints =
    case ints of
        [] ->
            -1

        head :: tail ->
            let
                complement =
                    target - head
            in
            if Set.member complement set then
                complement * head

            else
                let
                    newDict =
                        Set.insert head set
                in
                findTwo newDict target tail


findThree : Int -> Int -> Int -> Int -> OrderedDict Int Int -> Int
findThree pointer left right target ints =
    let
        valuePointer =
            fetchValue pointer ints

        valueLeft =
            fetchValue left ints

        valueRight =
            fetchValue right ints

        value =
            valuePointer + valueLeft + valueRight
    in
    if left >= right then
        findThree (pointer + 1) (pointer + 2) (OrderedDict.size ints - 1) target ints

    else if value > target then
        findThree pointer left (right - 1) target ints

    else if value < target then
        findThree pointer (left + 1) right target ints

    else
        valuePointer * valueLeft * valueRight


fetchValue : Int -> OrderedDict Int Int -> Int
fetchValue index dict =
    OrderedDict.get index dict |> Maybe.withDefault -100
