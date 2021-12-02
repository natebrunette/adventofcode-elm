module TupleExt exposing (product)


product : ( Int, Int ) -> Int
product ( first, second ) =
    first * second
