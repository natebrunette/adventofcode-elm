module Shared.List exposing (toIntList)


toIntList : List String -> List Int
toIntList strings =
    strings
        |> List.filterMap String.toInt
