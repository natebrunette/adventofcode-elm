module AOC.Year2021.Day10 exposing (part1, part2)

import Stack exposing (Stack)
import StringExt


part1 : List String -> String
part1 input =
    input
        |> List.map validateLine
        |> List.map
            (\( valid, stack ) ->
                if valid then
                    0

                else
                    case Stack.top stack |> Maybe.withDefault "" of
                        ")" ->
                            3

                        "]" ->
                            57

                        "}" ->
                            1197

                        ">" ->
                            25137

                        _ ->
                            0
            )
        |> List.sum
        |> String.fromInt


part2 : List String -> String
part2 input =
    input
        |> List.map validateLine
        |> List.filterMap
            (\( valid, stack ) ->
                if valid then
                    Just
                        (stack
                            |> Stack.toList
                            |> List.map mapChar
                            |> List.foldl (\val acc -> acc * 5 + val) 0
                        )

                else
                    Nothing
            )
        |> List.sort
        |> (\list -> list |> List.drop (List.length list // 2))
        |> List.head
        |> Maybe.withDefault 0
        |> String.fromInt


validateLine : String -> ( Bool, Stack String )
validateLine line =
    line
        |> StringExt.toStringList
        |> List.foldl parse ( True, Stack.initialise )


parse : String -> ( Bool, Stack String ) -> ( Bool, Stack String )
parse char ( valid, stack ) =
    let
        head =
            Stack.top stack |> Maybe.withDefault ""
    in
    if valid then
        case char of
            "]" ->
                if head == "[" then
                    ( True, Stack.pop stack |> Tuple.second )

                else
                    ( False, Stack.push char stack )

            ")" ->
                if head == "(" then
                    ( True, Stack.pop stack |> Tuple.second )

                else
                    ( False, Stack.push char stack )

            ">" ->
                if head == "<" then
                    ( True, Stack.pop stack |> Tuple.second )

                else
                    ( False, Stack.push char stack )

            "}" ->
                if head == "{" then
                    ( True, Stack.pop stack |> Tuple.second )

                else
                    ( False, Stack.push char stack )

            _ ->
                ( True, Stack.push char stack )

    else
        ( False, stack )


mapChar : String -> Int
mapChar string =
    case string of
        "(" ->
            1

        "[" ->
            2

        "{" ->
            3

        "<" ->
            4

        _ ->
            0
