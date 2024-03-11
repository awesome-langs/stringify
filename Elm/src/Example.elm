module Example exposing (..)
import Dict exposing (Dict)
import Posix.IO as IO exposing (..)
import Posix.IO.Process as Process
import Posix.IO.File as File
import Round

module Example exposing (..)

type alias PolyEvalType =
    { typeStr : String
    , typeName : String
    , valueType : Maybe PolyEvalType
    , keyType : Maybe PolyEvalType
    }

__newPolyEvalType : String -> String -> Maybe PolyEvalType -> Maybe PolyEvalType -> PolyEvalType
__newPolyEvalType typeStr typeName valueType keyType =
    { typeStr = typeStr, typeName = typeName, valueType = valueType, keyType = keyType }

__sToType : String -> PolyEvalType
__sToType typeStr =
    if not String.contains "<" typeStr Then
        __newPolyEvalType typeStr typeStr Nothing Nothing
    else
        let idx = String.index "<" typeStr |> Maybe.withDefault 0
            typeStr = String.slice 0 idx typeStr
            otherStr = String.slice (idx + 1) (String.length typeStr - 1) typeStr
        in if not String.contains "," otherStr Then
            let valueType = __sToType otherStr
            in __newPolyEvalType typeStr typeStr (Just valueType) Nothing
        else
            let idx = String.index "," otherStr |> Maybe.withDefault 0
                keyType = __sToType (String.slice 0 idx otherStr)
                valueType = __sToType (String.slice (idx + 1) (String.length otherStr) otherStr)
            in __newPolyEvalType typeStr typeStr (Just valueType) (Just keyType)

__escapeString : String -> String
__escapeString s =
    String.toList s |> List.map ( \c -> case c of
        '\\' -> "\\\\"
        '\"' -> "\\\""
        '\n' -> "\\n"
        '\t' -> "\\t"
        '\r' -> "\\r"
        _ -> String.fromChar c
    ) |> String.join ""

__byBool : Bool -> String
__byBool value =
    if value then "true" else "false"

__byInt : Int -> String
__byInt value =
    String.fromInt value

__byDouble : Float -> String
__byDouble value =
    let vs = Round 6 value
        vs = String.replace (Regex.regex "0+$") vs ""
        vs = String.replace (Regex.regex "\\.$") vs ".0"
    in if vs == "-0.0" then "0.0" else vs

__byString : String -> String
__byString value =
    "\"" ++ __escapeString value ++ "\""

__byList : List a -> PolyEvalType -> String
__byList value ty =
    let vStrs = List.map ( \v -> __valToS v ty.valueType ) value
    in "[" ++ String.join ", " vStrs ++ "]"

__byUList : List a -> PolyEvalType -> String
__byUList value ty =
    let vStrs = List.map ( \v -> __valToS v ty.valueType ) value
    in "[" ++ String.join ", " (List.sort vStrs) ++ "]"

__byDict : Dict comparable a -> PolyEvalType -> String
__byDict value ty =
    let vStrs = Dict.toList value |> List.map ( \(key, val) -> __valToS key ty.keyType ++ "=>" ++ __valToS val ty.valueType )
    in "{" ++ String.join ", " (List.sort vStrs) ++ "}"

__byOption : Maybe a -> PolyEvalType -> String
__byOption value ty =
    case value of
        Nothing -> "null"
        Just v -> __valToS v ty.valueType

__valToS : a -> Maybe PolyEvalType -> String
__valToS value ty =
    case ty of
        Just ty ->
            let typeName = ty.typeName
            in case typeName of
                "bool" -> if (value : Bool) then __byBool value else raise "Type mismatch"
                "int" -> if (value : Int) then __byInt value else raise "Type mismatch"
                "double" -> if (value : Float) then __byDouble value else raise "Type mismatch"
                "str" -> if (value : String) then __byString value else raise "Type mismatch"
                "list" -> if (value : List a) then __byList value ty else raise "Type mismatch"
                "ulist" -> if (value : List a) then __byUList value ty else raise "Type mismatch"
                "dict" -> if (value : Dict comparable a) then __byDict value ty else raise "Type mismatch"
                "option" -> __byOption value ty
                _ -> raise ("Unknown type " ++ typeName)
        Nothing -> raise "Type mismatch"

__stringify : a -> String -> String
__stringify value typeStr =
    __valToS value (Just (__sToType typeStr)) ++ ":" ++ typeStr

program : Process -> IO ()
program process =
    let tfs = __stringify True "bool" ++ "\n"
        ++ __stringify 3 "int" ++ "\n"
        ++ __stringify 3.141592653 "double" ++ "\n"
        ++ __stringify 3.0 "double" ++ "\n"
        ++ __stringify "Hello, World!" "str" ++ "\n"
        ++ __stringify "!@#$%^&*()\\\"\n\t" "str" ++ "\n"
        ++ __stringify [1, 2, 3] "list<int>" ++ "\n"
        ++ __stringify [True, False, True] "list<bool>" ++ "\n"
        ++ __stringify [3, 2, 1] "ulist<int>" ++ "\n"
        ++ __stringify (Dict.fromList [(1, "one"), (2, "two")]) "dict<int,str>" ++ "\n"
        ++ __stringify (Dict.fromList [("one", [1, 2, 3]), ("two", [4, 5, 6])]) "dict<str,list<int>>" ++ "\n"
        ++ __stringify Nothing "option<int>" ++ "\n"
        ++ __stringify (Just 3) "option<int>" ++ "\n"
    in File.writeContentsTo "stringify.out" tfs