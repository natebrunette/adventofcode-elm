module ListExt exposing (at, max, min, toIntList)

import List.Extra


toIntList : List String -> List Int
toIntList strings =
    strings
        |> List.filterMap String.toInt


at : Int -> List a -> a
at index list =
    case list |> List.Extra.getAt index of
        Just a ->
            a

        Nothing ->
            Debug.todo ("Could not get index at '" ++ (index |> String.fromInt) ++ "' of list " ++ Debug.toString list)


min : comparable -> List comparable -> comparable
min default list =
    list |> List.minimum |> Maybe.withDefault default


max : comparable -> List comparable -> comparable
max default list =
    list |> List.maximum |> Maybe.withDefault default
