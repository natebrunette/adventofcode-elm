module Shared.ListExt exposing (at, toIntList)

import List.Extra


toIntList : List String -> List Int
toIntList strings =
    strings
        |> List.filterMap String.toInt


at : Int -> a -> List a -> a
at index default list =
    list |> List.Extra.getAt index |> Maybe.withDefault default
