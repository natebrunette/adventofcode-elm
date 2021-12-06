module Grid exposing (Grid, colAll, findWhere, fromList, get, new, rowAll, set)

import Array exposing (Array)
import Array.Extra
import ArrayExt
import Comparable exposing (ifthen)
import List.Extra


type alias Grid a =
    { rows : Int
    , cols : Int
    , grid : Array (Array a)
    }


new : Int -> Int -> a -> Grid a
new rows cols default =
    Grid rows cols (Array.repeat rows (Array.repeat cols default))


fromList : Int -> List a -> Grid a
fromList cols list =
    let
        rows =
            (list |> List.length) // cols

        array =
            list |> List.Extra.groupsOf rows |> List.map Array.fromList |> Array.fromList
    in
    Grid rows cols array


get : Int -> Int -> Grid a -> a
get row col grid =
    grid.grid |> ArrayExt.at row |> ArrayExt.at col


set : Int -> Int -> (a -> a) -> Grid a -> Grid a
set row col setter grid =
    let
        newGrid =
            grid.grid |> Array.Extra.update row (Array.Extra.update col setter)
    in
    { grid | grid = newGrid }


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
