module AOC.Year2021.Day02 exposing (part1, part2)

import Parser exposing ((|.), (|=), Parser)


type Direction
    = Forward Int
    | Down Int
    | Up Int


type alias Position =
    { horizontal : Int
    , depth : Int
    , aim : Int
    }


newPosition : Position
newPosition =
    Position 0 0 0


positionProduct : Position -> Int
positionProduct position =
    position.horizontal * position.depth


part1 : List String -> String
part1 input =
    input
        |> List.map (Parser.run directionParser)
        |> List.map (Result.withDefault (Forward 0))
        |> List.foldl
            (\direction position ->
                case direction of
                    Forward num ->
                        { position | horizontal = position.horizontal + num }

                    Down num ->
                        { position | depth = position.depth + num }

                    Up num ->
                        { position | depth = position.depth - num }
            )
            newPosition
        |> positionProduct
        |> String.fromInt


part2 : List String -> String
part2 input =
    input
        |> List.map (Parser.run directionParser)
        |> List.map (Result.withDefault (Forward 0))
        |> List.foldl
            (\direction position ->
                case direction of
                    Forward num ->
                        { position | horizontal = position.horizontal + num, depth = position.depth + (position.aim * num) }

                    Down num ->
                        { position | aim = position.aim + num }

                    Up num ->
                        { position | aim = position.aim - num }
            )
            newPosition
        |> positionProduct
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
