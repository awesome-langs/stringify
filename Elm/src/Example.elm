module Example exposing (..)
import Dict exposing (Dict)
import Posix.IO as IO exposing (..)
import Posix.IO.Process as Process
import Posix.IO.File as File
import Round

myStringToInt : String -> Int
myStringToInt s =
    String.toInt s |> Maybe.withDefault 0

myStringToDouble : String -> Float
myStringToDouble s =
    String.toFloat s |> Maybe.withDefault 0.0

myIntToString : Int -> String
myIntToString i =
    String.fromInt i

myDoubleToString : Float -> String
myDoubleToString d =
    Round.round 6 d

myBoolToString : Bool -> String
myBoolToString b =
    if b then "true" else "false"

myIntToNullable : Int -> Maybe Int
myIntToNullable i =
    if i > 0 then Just i
    else if i < 0 then Just (-i)
    else Nothing

myNullableToInt : Maybe Int -> Int
myNullableToInt i =
    Maybe.withDefault -1 i

myListSorted : List String -> List String
myListSorted lst =
    List.sort lst

myListSortedByLength : List String -> List String
myListSortedByLength lst =
    List.sortBy String.length lst

myListFilter : List Int -> List Int
myListFilter lst =
    List.filter (\x -> modBy 3 x == 0) lst

myListMap : List Int -> List Int
myListMap lst =
    List.map (\x -> x * x) lst

myListReduce : List Int -> Int
myListReduce lst =
    List.foldl (\x acc -> acc * 10 + x) 0 lst

myListOperations : List Int -> Int
myListOperations lst =
    lst |> List.filter (\x -> modBy 3 x == 0)
        |> List.map (\x -> x * x)
        |> List.foldl (\x acc -> acc * 10 + x) 0

myListToDict : List Int -> Dict Int Int
myListToDict lst =
    lst |> List.map (\x -> (x, x * x)) |> Dict.fromList

myDictToList : Dict Int Int -> List Int
myDictToList dict =
    dict |> Dict.toList |> List.sortBy Tuple.first |> List.map (\x -> Tuple.first x + Tuple.second x)

myPrintString : String -> IO ()
myPrintString s =
    Process.print s

myPrintStringList : List String -> IO ()
myPrintStringList lst =
    List.foldr (\x acc -> 
        do ( File.write File.stdOut (x ++ " ") ) <| \_ -> acc) 
        (Process.print "") lst

myPrintIntList : List Int -> IO ()
myPrintIntList lst =
    lst |> List.map (\x -> myIntToString x) |> myPrintStringList

myPrintDict : Dict Int Int -> IO ()
myPrintDict dict =
    Dict.foldr (\k v acc -> 
        do ( File.write File.stdOut (myIntToString(k) ++ "->" ++ myIntToString(v) ++ " ") ) 
            <| \_ -> acc ) 
        (Process.print "") dict

program : Process -> IO ()
program process =
    do ( myPrintString("Hello, World!") ) <| \_ ->
    do ( myPrintString(myIntToString(myStringToInt("123"))) ) <| \_ ->
    do ( myPrintString(myDoubleToString(myStringToDouble("123.456"))) ) <| \_ ->
    do ( myPrintString(myBoolToString(False)) ) <| \_ ->
    do ( myPrintString(myIntToString(myNullableToInt(myIntToNullable(18)))) ) <| \_ ->
    do ( myPrintString(myIntToString(myNullableToInt(myIntToNullable(-15)))) ) <| \_ ->
    do ( myPrintString(myIntToString(myNullableToInt(myIntToNullable(0)))) ) <| \_ ->
    do ( myPrintStringList(myListSorted(["e", "dddd", "ccccc", "bb", "aaa"])) ) <| \_ ->
    do ( myPrintStringList(myListSortedByLength(["e", "dddd", "ccccc", "bb", "aaa"])) ) <| \_ ->
    do ( myPrintString(myIntToString(myListReduce(myListMap(myListFilter([3, 12, 5, 8, 9, 15, 7, 17, 21, 11]))))) ) <| \_ ->
    do ( myPrintString(myIntToString(myListOperations([3, 12, 5, 8, 9, 15, 7, 17, 21, 11]))) ) <| \_ ->
    do ( myPrintDict(myListToDict([3, 1, 4, 2, 5, 9, 8, 6, 7, 0])) ) <| \_ ->
    myPrintIntList(myDictToList(Dict.fromList([(3, 9), (1, 1), (4, 16), (2, 4), (5, 25), (9, 81), (8, 64), (6, 36), (7, 49), (0, 0)])))