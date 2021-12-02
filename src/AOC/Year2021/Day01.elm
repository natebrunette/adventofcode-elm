module AOC.Year2021.Day01 exposing (part1, part2)

import List.Extra
import ListExt


part1 : List String -> String
part1 input =
    input
        |> ListExt.toIntList
        |> calculateIncreasing
        |> String.fromInt


part2 : List String -> String
part2 input =
    input
        |> ListExt.toIntList
        |> List.Extra.groupsOfWithStep 3 1
        |> List.map List.sum
        |> calculateIncreasing
        |> String.fromInt


calculateIncreasing : List Int -> Int
calculateIncreasing list =
    let
        offsets =
            List.tail list |> Maybe.withDefault []
    in
    List.Extra.zip list offsets |> List.Extra.count (\( a, b ) -> b > a)
