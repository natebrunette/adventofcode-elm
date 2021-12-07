module AOC.Year2021.Day06 exposing (part1, part2)

import Dict exposing (Dict)
import DictExt
import List.Extra
import ListExt


part1 : List String -> String
part1 input =
    input
        |> ListExt.at 0
        |> String.split ","
        |> ListExt.toIntList
        |> List.foldl populateDict Dict.empty
        |> countFish 0 80
        |> String.fromInt


part2 : List String -> String
part2 input =
    input
        |> ListExt.at 0
        |> String.split ","
        |> ListExt.toIntList
        |> List.foldl populateDict Dict.empty
        |> countFish 0 256
        |> String.fromInt


populateDict : Int -> Dict Int Int -> Dict Int Int
populateDict num dict =
    dict |> DictExt.increment num


countFish : Int -> Int -> Dict Int Int -> Int
countFish days totalDays timers =
    if days == totalDays then
        timers |> Dict.values |> List.sum

    else
        let
            keys =
                timers |> Dict.keys |> List.sort |> List.Extra.remove -10 |> List.Extra.remove -20

            minimum =
                keys |> ListExt.at 0

            skipDays =
                if days + minimum + 1 > totalDays then
                    totalDays - days

                else
                    minimum + 1
        in
        keys
            |> List.foldl (updateKeys skipDays) timers
            |> (\dict -> dict |> DictExt.incrementBy 6 (dict |> DictExt.getWithDefault -10 0) |> Dict.remove -10)
            |> (\dict -> dict |> DictExt.incrementBy 8 (dict |> DictExt.getWithDefault -20 0) |> Dict.remove -20)
            |> countFish (days + skipDays) totalDays


updateKeys : Int -> Int -> Dict Int Int -> Dict Int Int
updateKeys skipDays key dict =
    dict
        |> Dict.get key
        |> Maybe.withDefault 0
        |> (\value ->
                if key - skipDays == -1 then
                    dict |> DictExt.incrementBy -10 value |> DictExt.incrementBy -20 value

                else
                    dict |> DictExt.incrementBy (key - skipDays) value
           )
        |> Dict.insert key 0
