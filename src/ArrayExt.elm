module ArrayExt exposing (at)

import Array exposing (Array)


at : Int -> Array a -> a
at index array =
    case array |> Array.get index of
        Just a ->
            a

        Nothing ->
            Debug.todo ("Could not get index at '" ++ (index |> String.fromInt) ++ "' of array " ++ Debug.toString array)
