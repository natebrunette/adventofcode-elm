module Comparable exposing (ifthen)


ifthen : Bool -> a -> a -> a
ifthen comparison trueResult falseResult =
    if comparison then
        trueResult

    else
        falseResult
