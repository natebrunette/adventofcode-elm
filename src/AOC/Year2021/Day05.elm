module AOC.Year2021.Day05 exposing (part1, part2)

import Dict exposing (Dict)
import DictExt
import List.Extra
import Parser exposing ((|.), (|=), Parser)


type alias Point =
    { x : Int
    , y : Int
    }


type alias Line =
    ( Point, Point )


part1 : List String -> String
part1 input =
    input
        |> getLines
        |> List.filter onlyVerticalOrHorizontal
        |> List.foldl addPointsBetweenLine Dict.empty
        |> Dict.filter (\_ value -> value > 1)
        |> Dict.size
        |> String.fromInt


part2 : List String -> String
part2 input =
    input
        |> getLines
        |> List.foldl addPointsBetweenLine Dict.empty
        |> Dict.filter (\_ value -> value > 1)
        |> Dict.size
        |> String.fromInt


addPointsBetweenLine : ( Point, Point ) -> Dict String Int -> Dict String Int
addPointsBetweenLine ( a, b ) dict =
    let
        ( point1, point2 ) =
            if a.x < b.x || (a.x == b.x && a.y < b.y) then
                ( a, b )

            else
                ( b, a )

        points =
            if point1.x == point2.x then
                List.range point1.y point2.y
                    |> List.Extra.zip (List.repeat (point2.y - point1.y + 1) point1.x)
                    |> List.map (\( x, y ) -> Point x y)

            else if point1.y == point2.y then
                List.repeat (point2.x - point1.x + 1) point1.y
                    |> List.Extra.zip (List.range point1.x point2.x)
                    |> List.map (\( x, y ) -> Point x y)

            else if point1.y < point2.y then
                createDiagonalPoints [ point1 ] point2 ( 1, 1 )

            else
                createDiagonalPoints [ point1 ] point2 ( 1, -1 )
    in
    points |> List.map pointToString |> List.foldl DictExt.increment dict


createDiagonalPoints : List Point -> Point -> ( Int, Int ) -> List Point
createDiagonalPoints points endingPoint ( dirX, dirY ) =
    case points of
        head :: _ ->
            if head.x == endingPoint.x && head.y == endingPoint.y then
                points

            else
                createDiagonalPoints (Point (head.x + dirX) (head.y + dirY) :: points) endingPoint ( dirX, dirY )

        [] ->
            points


pointToString : Point -> String
pointToString point =
    String.fromInt point.x ++ "," ++ String.fromInt point.y


onlyVerticalOrHorizontal : Line -> Bool
onlyVerticalOrHorizontal line =
    let
        firstPoint =
            Tuple.first line

        secondPoint =
            Tuple.second line
    in
    (firstPoint.x == secondPoint.x) || (firstPoint.y == secondPoint.y)


getLines : List String -> List Line
getLines strings =
    strings
        |> List.map (Parser.run lineParser)
        |> List.map (Result.withDefault ( Point 0 0, Point 0 0 ))


lineParser : Parser Line
lineParser =
    Parser.succeed Tuple.pair
        |= pointParser
        |. Parser.spaces
        |. Parser.token "->"
        |. Parser.spaces
        |= pointParser


pointParser : Parser Point
pointParser =
    Parser.succeed Point
        |= Parser.int
        |. Parser.token ","
        |= Parser.int
