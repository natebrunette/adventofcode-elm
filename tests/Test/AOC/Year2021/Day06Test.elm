module Test.AOC.Year2021.Day06Test exposing (suite)

import AOC.Year2021.Day06 exposing (part1, part2)
import Expect
import List.Extra
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "AOC 2021 - Day 06"
        [ test "part 1 sample" <|
            \_ ->
                Expect.equal "5934" (part1 (sample |> toList))
        , test "part 2 sample" <|
            \_ ->
                Expect.equal "26984457539" (part2 (sample |> toList))
        , test "part 1" <|
            \_ ->
                Expect.equal "356190" (part1 (input |> toList))
        , test "part 2" <|
            \_ ->
                Expect.equal "1617359101538" (part2 (input |> toList))
        ]


toList : String -> List String
toList string =
    string
        |> String.split "\n"
        |> List.map String.trim
        |> List.drop 1
        |> List.Extra.init
        |> Maybe.withDefault []


sample : String
sample =
    """
3,4,3,1,2
    """


input : String
input =
    """
1,2,5,1,1,4,1,5,5,5,3,4,1,2,2,5,3,5,1,3,4,1,5,2,5,1,4,1,2,2,1,5,1,1,1,2,4,3,4,2,2,4,5,4,1,2,3,5,3,4,1,1,2,2,1,3,3,2,3,2,1,2,2,3,1,1,2,5,1,2,1,1,3,1,1,5,5,4,1,1,5,1,4,3,5,1,3,3,1,1,5,2,1,2,4,4,5,5,4,4,5,4,3,5,5,1,3,5,2,4,1,1,2,2,2,4,1,2,1,5,1,3,1,1,1,2,1,2,2,1,3,3,5,3,4,2,1,5,2,1,4,1,1,5,1,1,5,4,4,1,4,2,3,5,2,5,5,2,2,4,4,1,1,1,4,4,1,3,5,4,2,5,5,4,4,2,2,3,2,1,3,4,1,5,1,4,5,2,4,5,1,3,4,1,4,3,3,1,1,3,2,1,5,5,3,1,1,2,4,5,3,1,1,1,2,5,2,4,5,1,3,2,4,5,5,1,2,3,4,4,1,4,1,1,3,3,5,1,2,5,1,2,5,4,1,1,3,2,1,1,1,3,5,1,3,2,4,3,5,4,1,1,5,3,4,2,3,1,1,4,2,1,2,2,1,1,4,3,1,1,3,5,2,1,3,2,1,1,1,2,1,1,5,1,1,2,5,1,1,4
    """
