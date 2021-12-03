module AOC.Year2021.Day03 exposing (part1, part2)

import Binary
import Comparable exposing (ifthen)
import List.Extra
import ListExt
import StringExt


part1 : List String -> String
part1 input =
    let
        gamma =
            input |> getMostCommonBits |> Binary.fromIntegers

        epsilon =
            gamma |> Binary.not
    in
    ((gamma |> Binary.toDecimal) * (epsilon |> Binary.toDecimal))
        |> String.fromInt


part2 : List String -> String
part2 input =
    let
        gamma =
            input
                |> List.map StringExt.toStringList
                |> filterList 0 getMostCommonBit
                |> List.map StringExt.toInt
                |> Binary.fromIntegers
                |> Binary.toDecimal

        epsilon =
            input
                |> List.map StringExt.toStringList
                |> filterList 0 getLeastCommonBit
                |> List.map StringExt.toInt
                |> Binary.fromIntegers
                |> Binary.toDecimal
    in
    gamma * epsilon |> String.fromInt


getMostCommonBits : List String -> List Int
getMostCommonBits strings =
    strings
        |> List.map StringExt.toStringList
        |> List.Extra.transpose
        |> List.map getMostCommonBit


getMostCommonBit : List String -> Int
getMostCommonBit strings =
    strings
        |> List.foldl (\num ( ones, zeros ) -> ifthen (num == "1") ( ones + 1, zeros ) ( ones, zeros + 1 )) ( 0, 0 )
        |> (\( a, b ) -> ifthen (a >= b) 1 0)


getLeastCommonBit : List String -> Int
getLeastCommonBit strings =
    strings |> getMostCommonBit |> Binary.fromDecimal |> Binary.not |> Binary.toDecimal


filterList : Int -> (List String -> Int) -> List (List String) -> List String
filterList index bitFinder lists =
    if List.length lists == 1 then
        lists |> ListExt.at 0

    else
        let
            bit =
                lists |> List.Extra.transpose |> ListExt.at index |> bitFinder |> String.fromInt
        in
        lists
            |> List.filter (\list -> ListExt.at index list == bit)
            |> filterList (index + 1) bitFinder
