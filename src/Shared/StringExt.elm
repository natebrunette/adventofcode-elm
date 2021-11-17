module Shared.StringExt exposing (toInt, toStringList)


toInt : Int -> String -> Int
toInt default string =
    string |> String.toInt |> Maybe.withDefault default


toStringList : String -> List String
toStringList string =
    string |> String.toList |> List.map String.fromChar
