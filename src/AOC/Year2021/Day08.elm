module AOC.Year2021.Day08 exposing (part1, part2)

import Dict exposing (Dict)
import DictExt
import List.Extra
import ListExt
import Set exposing (Set)
import Set.Extra
import StringExt
import TupleExt


type alias LengthMap =
    Dict Int (List (Set String))


type alias NumberMap =
    Dict Int (Set String)


part1 : List String -> String
part1 input =
    input
        |> List.concatMap
            (String.split " | "
                >> ListExt.at 1
                >> String.words
                >> List.filterMap
                    (\string ->
                        let
                            length =
                                string |> String.length
                        in
                        if length == 2 || length == 3 || length == 4 || length == 7 then
                            Just string

                        else
                            Nothing
                    )
            )
        |> List.length
        |> String.fromInt


part2 : List String -> String
part2 input =
    input
        |> List.map (splitString >> (\( sample, code ) -> sample |> parseSample |> parseCode code))
        |> List.sum
        |> String.fromInt


splitString : String -> ( List (Set String), List (Set String) )
splitString string =
    string
        |> String.split " | "
        |> List.map (String.words >> List.map (StringExt.toStringList >> Set.fromList))
        |> TupleExt.fromList


parseSample : List (Set String) -> NumberMap
parseSample strings =
    let
        lengthMap : LengthMap
        lengthMap =
            strings
                |> List.filterMap
                    (\set ->
                        let
                            length =
                                Set.size set
                        in
                        if length == 2 || length == 3 || length == 4 || length == 7 then
                            Nothing

                        else
                            Just ( length, set )
                    )
                |> List.foldl
                    (\( length, set ) dict ->
                        dict |> Dict.insert length (DictExt.getWithDefault length [] dict |> (::) set)
                    )
                    Dict.empty

        lengths =
            Dict.fromList
                [ ( 1, strings |> fetchNumber 2 )
                , ( 4, strings |> fetchNumber 4 )
                , ( 7, strings |> fetchNumber 3 )
                , ( 8, strings |> fetchNumber 7 )
                ]
    in
    Tuple.pair lengthMap lengths
        |> parseNine
        |> parseSix
        |> parseZero
        |> parseThree
        |> parseTwo
        |> parseFive
        |> Tuple.second


topRight : NumberMap -> Set String
topRight numberMap =
    Set.diff (DictExt.get 8 numberMap) (DictExt.get 6 numberMap)


fourAndSeven : NumberMap -> Set String
fourAndSeven numberMap =
    Set.union (DictExt.get 4 numberMap) (DictExt.get 7 numberMap)


parseNine : ( LengthMap, NumberMap ) -> ( LengthMap, NumberMap )
parseNine ( lengthMap, numberMap ) =
    lengthMap
        |> DictExt.get 6
        |> List.filter (\item -> Set.diff item (fourAndSeven numberMap) |> Set.size |> (==) 1)
        |> ListExt.at 0
        |> (\nine -> ( updateLengthMap 6 nine lengthMap, Dict.insert 9 nine numberMap ))


parseSix : ( LengthMap, NumberMap ) -> ( LengthMap, NumberMap )
parseSix ( lengthMap, numberMap ) =
    lengthMap
        |> DictExt.get 6
        |> List.filter (\item -> Set.Extra.subset (DictExt.get 1 numberMap) item |> not)
        |> ListExt.at 0
        |> (\six -> ( updateLengthMap 6 six lengthMap, Dict.insert 6 six numberMap ))


parseZero : ( LengthMap, NumberMap ) -> ( LengthMap, NumberMap )
parseZero ( lengthMap, numberMap ) =
    lengthMap
        |> DictExt.get 6
        |> ListExt.at 0
        |> (\zero -> ( updateLengthMap 6 zero lengthMap, Dict.insert 0 zero numberMap ))


parseThree : ( LengthMap, NumberMap ) -> ( LengthMap, NumberMap )
parseThree ( lengthMap, numberMap ) =
    lengthMap
        |> DictExt.get 5
        |> List.filter (\item -> Set.Extra.subset (DictExt.get 1 numberMap) item)
        |> ListExt.at 0
        |> (\three -> ( updateLengthMap 5 three lengthMap, Dict.insert 3 three numberMap ))


parseTwo : ( LengthMap, NumberMap ) -> ( LengthMap, NumberMap )
parseTwo ( lengthMap, numberMap ) =
    lengthMap
        |> DictExt.get 5
        |> List.filter (\item -> Set.intersect item (topRight numberMap) |> Set.size |> (==) 1)
        |> ListExt.at 0
        |> (\two -> ( updateLengthMap 5 two lengthMap, Dict.insert 2 two numberMap ))


parseFive : ( LengthMap, NumberMap ) -> ( LengthMap, NumberMap )
parseFive ( lengthMap, numberMap ) =
    lengthMap
        |> DictExt.get 5
        |> ListExt.at 0
        |> (\five -> ( updateLengthMap 5 five lengthMap, Dict.insert 5 five numberMap ))


updateLengthMap : Int -> Set String -> LengthMap -> LengthMap
updateLengthMap key num lengthMap =
    lengthMap |> Dict.insert key (List.Extra.remove num (DictExt.get key lengthMap))


parseCode : List (Set String) -> NumberMap -> Int
parseCode strings numberMap =
    strings
        |> List.map
            (\code ->
                numberMap
                    |> Dict.filter
                        (\_ num ->
                            (Set.size code == Set.size num) && (Set.diff code num |> Set.isEmpty)
                        )
                    |> Dict.toList
                    |> ListExt.at 0
                    |> Tuple.first
                    |> String.fromInt
            )
        |> String.concat
        |> StringExt.toInt


fetchNumber : Int -> List (Set String) -> Set String
fetchNumber size sets =
    sets |> List.Extra.find (Set.size >> (==) size) |> Maybe.withDefault Set.empty
