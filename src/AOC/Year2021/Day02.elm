module AOC.Year2021.Day02 exposing (part1, part2)

import Parser exposing ((|.), (|=), Parser)
import Shared.TupleExt as TupleExt


type Direction
    = Forward Int
    | Down Int
    | Up Int


part1 : List String -> String
part1 input =
    input
        |> List.map (Parser.run directionParser)
        |> List.map (Result.withDefault (Forward 0))
        |> List.foldl
            (\direction acc ->
                case direction of
                    Forward num ->
                        Tuple.mapFirst ((+) num) acc

                    Down num ->
                        Tuple.mapSecond ((+) num) acc

                    Up num ->
                        Tuple.mapSecond (\val -> val - num) acc
            )
            ( 0, 0 )
        |> TupleExt.product
        |> String.fromInt


part2 : List String -> String
part2 input =
    input
        |> List.map (Parser.run directionParser)
        |> List.map (Result.withDefault (Forward 0))
        |> List.foldl
            (\direction acc ->
                let
                    position =
                        Tuple.first acc

                    aim =
                        Tuple.second acc
                in
                case direction of
                    Forward num ->
                        ( Tuple.mapBoth ((+) num) ((+) (num * aim)) position, aim )

                    Down num ->
                        ( position, aim + num )

                    Up num ->
                        ( position, aim - num )
            )
            ( ( 0, 0 ), 0 )
        |> Tuple.first
        |> TupleExt.product
        |> String.fromInt


directionParser : Parser Direction
directionParser =
    Parser.oneOf
        [ Parser.map (\_ -> Forward) (Parser.token "forward")
        , Parser.map (\_ -> Down) (Parser.token "down")
        , Parser.map (\_ -> Up) (Parser.token "up")
        ]
        |. Parser.spaces
        |= Parser.int
