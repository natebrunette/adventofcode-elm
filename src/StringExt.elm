module StringExt exposing (toInt, toStringList)


toInt : String -> Int
toInt string =
    case string |> String.toInt of
        Just a ->
            a

        Nothing ->
            Debug.todo ("Could convert '" ++ string ++ "' to int")


toStringList : String -> List String
toStringList string =
    string |> String.toList |> List.map String.fromChar
