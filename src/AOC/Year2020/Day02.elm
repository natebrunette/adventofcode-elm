module AOC.Year2020.Day02 exposing (part1, part2)

import Dict exposing (Dict)
import ListExt
import StringExt


type alias PasswordRule =
    { counts : Dict Char Int
    , min : Int
    , max : Int
    }


part1 : List String -> String
part1 input =
    input
        |> List.filter validPassword
        |> List.length
        |> String.fromInt


part2 : List String -> String
part2 input =
    input
        |> List.filter validPasswordFixed
        |> List.length
        |> String.fromInt


validPassword : String -> Bool
validPassword string =
    let
        stringParts =
            string
                |> String.split " "

        minMax =
            stringParts |> ListExt.at 0 |> String.split "-"

        min =
            minMax |> ListExt.at 0 |> StringExt.toInt

        max =
            minMax |> ListExt.at 1 |> StringExt.toInt

        search =
            stringParts |> ListExt.at 1 |> String.left 1

        password =
            stringParts |> ListExt.at 2

        count =
            password |> StringExt.toStringList |> List.filter (\char -> char == search) |> List.length
    in
    count <= max && count >= min


validPasswordFixed : String -> Bool
validPasswordFixed string =
    let
        stringParts =
            string
                |> String.split " "

        firstSecond =
            stringParts |> ListExt.at 0 |> String.split "-"

        firstIndex =
            firstSecond |> ListExt.at 0 |> StringExt.toInt

        secondIndex =
            firstSecond |> ListExt.at 1 |> StringExt.toInt

        search =
            stringParts |> ListExt.at 1 |> String.left 1

        password =
            stringParts |> ListExt.at 2

        passwordList =
            password |> StringExt.toStringList

        firstCorrect =
            ListExt.at (firstIndex - 1) passwordList == search

        secondCorrect =
            ListExt.at (secondIndex - 1) passwordList == search
    in
    xor firstCorrect secondCorrect
