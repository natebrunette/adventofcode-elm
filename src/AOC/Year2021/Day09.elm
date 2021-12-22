module AOC.Year2021.Day09 exposing (part1, part2)

import Grid exposing (Grid)
import ListExt
import Set exposing (Set)
import StringExt


part1 : List String -> String
part1 input =
    input
        |> List.map (StringExt.toStringList >> ListExt.toIntList)
        |> Grid.fromLists
        |> findLowPoints
        |> List.map ((+) 1)
        |> List.sum
        |> String.fromInt


part2 : List String -> String
part2 input =
    input
        |> List.map (StringExt.toStringList >> ListExt.toIntList)
        |> Grid.fromLists
        |> findBasinSizes
        |> List.sort
        |> List.reverse
        |> List.take 3
        |> List.foldl (*) 1
        |> String.fromInt


isLowPoint : Int -> Int -> Int -> Grid Int -> Bool
isLowPoint row col val grid =
    grid |> Grid.getAdjacent row col |> List.all (.val >> (<) val)


findLowPoints : Grid Int -> List Int
findLowPoints grid =
    grid
        |> Grid.filter
            (\val row col ->
                isLowPoint row col val grid
            )


findBasinSizes : Grid Int -> List Int
findBasinSizes grid =
    grid
        |> Grid.map
            (\val row col ->
                if isLowPoint row col val grid then
                    adjacentCount row col val grid Set.empty |> Set.size |> (+) 1

                else
                    0
            )
        |> Grid.toFlatList


adjacentCount : Int -> Int -> Int -> Grid Int -> Set Int -> Set Int
adjacentCount row col current grid visited =
    let
        adjacent =
            grid
                |> Grid.getAdjacent row col
                |> List.filter (\{ id } -> visited |> Set.member id |> not)
                |> List.filter (.val >> (\adjVal -> current < adjVal && adjVal /= 9))
    in
    case adjacent of
        [] ->
            visited

        list ->
            list
                |> List.foldl
                    (\cell visited_ ->
                        visited_ |> Set.union (adjacentCount cell.row cell.col cell.val grid (Set.union visited (Set.singleton cell.id)))
                    )
                    visited
