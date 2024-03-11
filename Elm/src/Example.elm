module Example exposing (..)
import Dict exposing (Dict)
import Posix.IO as IO exposing (..)
import Posix.IO.Process as Process
import Posix.IO.File as File
import Round

type alias PolyEvalType =
    { typeStr : String, typeName : String, valueType : Maybe PolyEvalType, keyType : Maybe PolyEvalType}

newPolyEvalType__ : String -> String -> Maybe PolyEvalType -> Maybe PolyEvalType -> PolyEvalType
newPolyEvalType__ typeStr typeName valueType keyType =
    { typeStr = typeStr, typeName = typeName, valueType = valueType, keyType = keyType }

sToType__ : String -> PolyEvalType
sToType__ typeStr =
    if not String.contains "<" typeStr then
        newPolyEvalType__ typeStr typeStr Nothing Nothing
    else
        let idx = String.index "<" typeStr |> Maybe.withDefault 0
            typeStr = String.slice 0 idx typeStr
            otherStr = String.slice (idx + 1) (String.length typeStr - 1) typeStr
        in if not String.contains "," otherStr then
            let valueType = sToType__ otherStr
            in newPolyEvalType__ typeStr typeStr (Just valueType) Nothing
        else
            let idx = String.index "," otherStr |> Maybe.withDefault 0
                keyType = sToType__ (String.slice 0 idx otherStr)
                valueType = sToType__ (String.slice (idx + 1) (String.length otherStr) otherStr)
            in newPolyEvalType__ typeStr typeStr (Just valueType) (Just keyType)

escapeString__ : String -> String
escapeString__ s =
    String.toList s |> List.map ( \c -> case c of
        '\\' -> "\\\\"
        '\"' -> "\\\""
        '\n' -> "\\n"
        '\t' -> "\\t"
        '\r' -> "\\r"
        _ -> String.fromChar c
    ) |> String.join ""

byBool__ : Bool -> String
byBool__ value =
    if value then "true" else "false"

byInt__ : Int -> String
byInt__ value =
    String.fromInt value

byDouble__ : Float -> String
byDouble__ value =
    let vs = Round 6 value
        vs = String.replace (Regex.regex "0+$") vs ""
        vs = String.replace (Regex.regex "\\.$") vs ".0"
    in if vs == "-0.0" then "0.0" else vs

byString__ : String -> String
byString__ value =
    "\"" ++ escapeString__ value ++ "\""

byList__ : List a -> PolyEvalType -> String
byList__ value ty =
    let vStrs = List.map ( \v -> valToS__ v ty.valueType ) value
    in "[" ++ String.join ", " vStrs ++ "]"

byUList__ : List a -> PolyEvalType -> String
byUList__ value ty =
    let vStrs = List.map ( \v -> valToS__ v ty.valueType ) value
    in "[" ++ String.join ", " (List.sort vStrs) ++ "]"

byDict__ : Dict comparable a -> PolyEvalType -> String
byDict__ value ty =
    let vStrs = Dict.toList value |> List.map ( \(key, val) -> valToS__ key ty.keyType ++ "=>" ++ valToS__ val ty.valueType )
    in "{" ++ String.join ", " (List.sort vStrs) ++ "}"

byOption__ : Maybe a -> PolyEvalType -> String
byOption__ value ty =
    case value of
        Nothing -> "null"
        Just v -> valToS__ v ty.valueType

valToS__ : a -> Maybe PolyEvalType -> String
valToS__ value ty =
    case ty of
        Just ty ->
            let typeName = ty.typeName
            in case typeName of
                "bool" -> if (value : Bool) then byBool__ value else raise "Type mismatch"
                "int" -> if (value : Int) then byInt__ value else raise "Type mismatch"
                "double" -> if (value : Float) then byDouble__ value else raise "Type mismatch"
                "str" -> if (value : String) then byString__ value else raise "Type mismatch"
                "list" -> if (value : List a) then byList__ value ty else raise "Type mismatch"
                "ulist" -> if (value : List a) then byUList__ value ty else raise "Type mismatch"
                "dict" -> if (value : Dict comparable a) then byDict__ value ty else raise "Type mismatch"
                "option" -> byOption__ value ty
                _ -> raise ("Unknown type " ++ typeName)
        Nothing -> raise "Type mismatch"

stringify__ : a -> String -> String
stringify__ value typeStr =
    valToS__ value (Just (sToType__ typeStr)) ++ ":" ++ typeStr

program : Process -> IO ()
program process =
    let tfs = stringify__ True "bool" ++ "\n"
        ++ stringify__ 3 "int" ++ "\n"
        ++ stringify__ 3.141592653 "double" ++ "\n"
        ++ stringify__ 3.0 "double" ++ "\n"
        ++ stringify__ "Hello, World!" "str" ++ "\n"
        ++ stringify__ "!@#$%^&*()\\\"\n\t" "str" ++ "\n"
        ++ stringify__ [1, 2, 3] "list<int>" ++ "\n"
        ++ stringify__ [True, False, True] "list<bool>" ++ "\n"
        ++ stringify__ [3, 2, 1] "ulist<int>" ++ "\n"
        ++ stringify__ (Dict.fromList [(1, "one"), (2, "two")]) "dict<int,str>" ++ "\n"
        ++ stringify__ (Dict.fromList [("one", [1, 2, 3]), ("two", [4, 5, 6])]) "dict<str,list<int>>" ++ "\n"
        ++ stringify__ Nothing "option<int>" ++ "\n"
        ++ stringify__ (Just 3) "option<int>" ++ "\n"
    in File.writeContentsTo "stringify.out" tfs