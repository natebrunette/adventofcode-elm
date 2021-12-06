module AOC.Year2021.Day04 exposing (part1, part2)

import Array
import Comparable exposing (ifthen)
import Grid exposing (Grid)
import List.Extra
import StringExt


type alias Board =
    Grid ( Bool, Int )


type Found
    = Found Int Board
    | NotFound (List Board)


type FoundLatest
    = FoundLatest Int Board (List Board)
    | NotFoundLatest (List Board)


part1 : List String -> String
part1 input =
    let
        ( numbers, boards ) =
            case input of
                numbersString :: boardsString ->
                    ( String.split "," numbersString |> List.map StringExt.toInt, createBoards boardsString )

                [] ->
                    ( [], [] )
    in
    numbers
        |> List.foldl findWinningBoard (NotFound boards)
        |> (\result ->
                case result of
                    Found num board ->
                        num * unmarkedTotal board

                    NotFound _ ->
                        0
           )
        |> String.fromInt


part2 : List String -> String
part2 input =
    let
        ( numbers, boards ) =
            case input of
                numbersString :: boardsString ->
                    ( String.split "," numbersString |> List.map StringExt.toInt, createBoards boardsString )

                [] ->
                    ( [], [] )
    in
    numbers
        |> List.foldl findLastWinningBoard (NotFoundLatest boards)
        |> (\result ->
                case result of
                    FoundLatest num board _ ->
                        num * unmarkedTotal board

                    NotFoundLatest _ ->
                        0
           )
        |> String.fromInt


createBoards : List String -> List Board
createBoards boardInput =
    boardInput
        |> StringExt.groupOnDoubleNewline
        |> List.map (String.words >> List.map (StringExt.toInt >> Tuple.pair False) >> Grid.fromList 5)


findWinningBoard : Int -> Found -> Found
findWinningBoard num found =
    case found of
        Found _ _ ->
            found

        NotFound boards ->
            let
                newBoards =
                    boards |> List.map (markBoard num)
            in
            case newBoards |> List.Extra.find (Tuple.first >> (==) True) of
                Just ( _, board ) ->
                    Found num board

                Nothing ->
                    NotFound (newBoards |> List.map Tuple.second)


findLastWinningBoard : Int -> FoundLatest -> FoundLatest
findLastWinningBoard num found =
    let
        nonWinningFilter =
            List.filter (Tuple.first >> (==) False) >> List.map Tuple.second
    in
    case found of
        FoundLatest originalNum originalBoard boards ->
            let
                newBoards =
                    boards |> List.map (markBoard num)

                nonWinningBoards =
                    newBoards |> nonWinningFilter
            in
            case newBoards |> List.reverse |> List.Extra.find (Tuple.first >> (==) True) of
                Just ( _, board ) ->
                    FoundLatest num board nonWinningBoards

                Nothing ->
                    FoundLatest originalNum originalBoard nonWinningBoards

        NotFoundLatest boards ->
            let
                newBoards =
                    boards |> List.map (markBoard num)

                nonWinningBoards =
                    newBoards |> nonWinningFilter
            in
            case newBoards |> List.Extra.find (Tuple.first >> (==) True) of
                Just ( _, board ) ->
                    FoundLatest num board nonWinningBoards

                Nothing ->
                    NotFoundLatest nonWinningBoards


markBoard : Int -> Board -> ( Bool, Board )
markBoard num board =
    let
        cell =
            board |> Grid.findWhere (Tuple.second >> (==) num)
    in
    case cell of
        Just ( row, col ) ->
            let
                newBoard =
                    board |> Grid.set row col (Tuple.mapFirst (\_ -> True))

                isWinning =
                    newBoard |> isBoardWinning row col
            in
            ( isWinning, newBoard )

        Nothing ->
            ( False, board )


isBoardWinning : Int -> Int -> Board -> Bool
isBoardWinning row col board =
    if Grid.rowAll (\val -> Tuple.first val == True) row board then
        True

    else if Grid.colAll (\val -> Tuple.first val == True) col board then
        True

    else
        False


unmarkedTotal : Board -> Int
unmarkedTotal board =
    board.grid
        |> Array.map (Array.foldl (\( marked, num ) total -> ifthen marked total (num + total)) 0)
        |> Array.foldl (+) 0
