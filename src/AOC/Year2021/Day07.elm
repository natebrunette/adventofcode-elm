module AOC.Year2021.Day07 exposing (part1, part2)

import Dict exposing (Dict)
import DictExt
import ListExt
import String


part1 : List String -> String
part1 input =
    let
        list =
            input
                |> ListExt.at 0
                |> String.split ","
                |> ListExt.toIntList
    in
    list
        |> List.foldl (populateDict list) Dict.empty
        |> Dict.values
        |> ListExt.min 0
        |> String.fromInt


part2 : List String -> String
part2 input =
    let
        list =
            input
                |> ListExt.at 0
                |> String.split ","
                |> ListExt.toIntList

        range =
            List.range (list |> ListExt.min 0) (list |> ListExt.max 0)
    in
    range
        |> List.foldl (populateDictIncreasing list) Dict.empty
        |> Dict.values
        |> ListExt.min 0
        |> String.fromInt


populateDict : List Int -> Int -> Dict Int Int -> Dict Int Int
populateDict list key dict =
    case Dict.get key dict of
        Just _ ->
            dict

        Nothing ->
            list
                |> List.foldl (incrementDict calcFuel key) dict


populateDictIncreasing : List Int -> Int -> Dict Int Int -> Dict Int Int
populateDictIncreasing list key dict =
    case Dict.get key dict of
        Just _ ->
            dict

        Nothing ->
            list
                |> List.foldl (incrementDict calcFuelIncreasing key) dict


incrementDict : (Int -> Int -> Int) -> Int -> Int -> Dict Int Int -> Dict Int Int
incrementDict fuelCalculator baseKey compareKey dict =
    dict |> DictExt.incrementBy baseKey (fuelCalculator baseKey compareKey)


calcFuel : Int -> Int -> Int
calcFuel a b =
    a - b |> abs


calcFuelIncreasing : Int -> Int -> Int
calcFuelIncreasing a b =
    let
        maximum =
            a - b |> abs
    in
    (maximum ^ 2 + maximum) // 2
