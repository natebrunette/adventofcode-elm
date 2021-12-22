module Grid exposing (Grid, colAll, filter, findWhere, fromList, fromLists, get, getAdjacent, map, rowAll, set, toFlatList)

import Array exposing (Array)
import Array.Extra
import ArrayExt
import Comparable exposing (ifthen)
import List.Extra
import ListExt
import Maybe.Extra


type alias Grid a =
    { rows : Int
    , cols : Int
    , grid : Array (Array a)
    }


type alias Cell a =
    { id : Int
    , row : Int
    , col : Int
    , val : a
    }


new : Int -> Int -> Array (Array a) -> Grid a
new rows cols arrays =
    Grid rows cols arrays


fromList : Int -> List a -> Grid a
fromList cols list =
    let
        rows =
            (list |> List.length) // cols

        array =
            list |> List.Extra.groupsOf rows |> List.map Array.fromList |> Array.fromList
    in
    Grid rows cols array


fromLists : List (List a) -> Grid a
fromLists lists =
    let
        rows =
            lists |> List.length

        cols =
            lists |> ListExt.at 0 |> List.length
    in
    lists
        |> List.map Array.fromList
        |> Array.fromList
        |> Grid rows cols


toFlatList : Grid a -> List a
toFlatList grid =
    grid.grid
        |> Array.map Array.toList
        |> Array.toList
        |> List.concat


get : Int -> Int -> Grid a -> a
get row col grid =
    grid.grid |> ArrayExt.at row |> ArrayExt.at col


set : Int -> Int -> (a -> a) -> Grid a -> Grid a
set row col setter grid =
    let
        newGrid =
            grid.grid
                |> Array.Extra.update row (Array.Extra.update col setter)
    in
    { grid | grid = newGrid }


getAdjacent : Int -> Int -> Grid a -> List (Cell a)
getAdjacent row col grid =
    [ ( row - 1, col )
    , ( row + 1, col )
    , ( row, col - 1 )
    , ( row, col + 1 )
    ]
        |> List.filter (\( newRow, newCol ) -> newRow >= 0 && newRow < grid.rows && newCol >= 0 && newCol < grid.cols)
        |> List.map (\( newRow, newCol ) -> grid |> get newRow newCol |> Cell (newRow * grid.cols + newCol) newRow newCol)


map : (a -> Int -> Int -> b) -> Grid a -> Grid b
map mapper grid =
    grid.grid
        |> Array.indexedMap
            (\rowIndex row ->
                row
                    |> Array.indexedMap
                        (\colIndex _ ->
                            let
                                current =
                                    get rowIndex colIndex grid
                            in
                            mapper current rowIndex colIndex
                        )
            )
        |> new grid.rows grid.cols


filter : (a -> Int -> Int -> Bool) -> Grid a -> List a
filter isGood grid =
    grid.grid
        |> Array.indexedMap
            (\rowIndex row ->
                row
                    |> Array.indexedMap
                        (\colIndex _ ->
                            let
                                current =
                                    get rowIndex colIndex grid
                            in
                            if isGood current rowIndex colIndex then
                                Just current

                            else
                                Nothing
                        )
                    |> Array.toList
                    |> Maybe.Extra.values
            )
        |> Array.toList
        |> List.concat


findWhere : (a -> Bool) -> Grid a -> Maybe ( Int, Int )
findWhere filterFn grid =
    grid.grid
        |> Array.indexedMap
            (\rowIndex row ->
                Array.indexedMap
                    (\colIndex val ->
                        if filterFn val then
                            Just ( rowIndex, colIndex )

                        else
                            Nothing
                    )
                    row
                    |> Array.Extra.filterMap identity
            )
        |> Array.Extra.filterMap (\arr -> ifthen (Array.isEmpty arr) Nothing (Just arr))
        |> Array.get 0
        |> Maybe.withDefault Array.empty
        |> Array.get 0


rowAll : (a -> Bool) -> Int -> Grid a -> Bool
rowAll predicate row grid =
    grid.grid |> ArrayExt.at row |> Array.Extra.all predicate


colAll : (a -> Bool) -> Int -> Grid a -> Bool
colAll predicate col grid =
    grid |> getCol col |> Array.Extra.all predicate


getCol : Int -> Grid a -> Array a
getCol col grid =
    grid.grid
        |> Array.map (ArrayExt.at col)
