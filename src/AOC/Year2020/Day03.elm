module AOC.Year2020.Day03 exposing (part1, part2)

import Shared.ListExt as ListExt


type alias Grid =
    List (List Char)


part1 : List String -> String
part1 input =
    input
        |> toGrid
        |> countTrees 0 3 0
        |> String.fromInt


part2 : List String -> String
part2 input =
    let
        grid =
            input |> toGrid
    in
    [ ( 1, 1 ), ( 3, 1 ), ( 5, 1 ), ( 7, 1 ), ( 1, 2 ) ]
        |> List.map
            (\( right, down ) ->
                grid
                    |> countTrees (down - 1) right 0
            )
        |> List.foldl (*) 1
        |> String.fromInt


toGrid : List String -> Grid
toGrid strings =
    strings |> List.map String.toList


countTrees : Int -> Int -> Int -> Grid -> Int
countTrees drop right position grid =
    case grid of
        [] ->
            0

        head :: tail ->
            let
                tree =
                    if ListExt.at position head == '#' then
                        1

                    else
                        0

                length =
                    List.length head

                nextPosition =
                    if position + right >= length then
                        position + right - length

                    else
                        position + right

                newTail =
                    List.drop drop tail
            in
            tree + countTrees drop right nextPosition newTail
