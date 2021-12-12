module TupleExt exposing (fromList, product)

import ListExt


product : ( Int, Int ) -> Int
product ( first, second ) =
    first * second


fromList : List a -> ( a, a )
fromList list =
    list |> (\list_ -> ( ListExt.at 0 list_, ListExt.at 1 list_ ))
