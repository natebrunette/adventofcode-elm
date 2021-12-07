module DictExt exposing (getWithDefault, increment, incrementBy)

import Dict exposing (Dict)


increment : comparable -> Dict comparable Int -> Dict comparable Int
increment key dict =
    dict |> incrementBy key 1


incrementBy : comparable -> Int -> Dict comparable Int -> Dict comparable Int
incrementBy key incVal dict =
    dict |> getWithDefault key 0 |> (\value -> Dict.insert key (value + incVal) dict)


getWithDefault : comparable -> v -> Dict comparable v -> v
getWithDefault key default dict =
    dict |> Dict.get key |> Maybe.withDefault default
