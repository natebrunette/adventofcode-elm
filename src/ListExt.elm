module ListExt exposing (at, toIntList)

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
