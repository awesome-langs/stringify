module Example exposing (..)
import Dict exposing (Dict)
import Posix.IO as IO exposing (..)
import Posix.IO.Process as Process
import Posix.IO.File as File
import Round

type alias PolyEvalType =
    { typeStr : String, typeName : String, valueType : PolyEvalTypeRef, keyType : PolyEvalTypeRef }

type PolyEvalTypeRef = PolyEvalTypeRef ( Maybe PolyEvalType )

newPolyEvalType__ : String -> String -> PolyEvalTypeRef -> PolyEvalTypeRef -> PolyEvalType
newPolyEvalType__ typeStr typeName valueType keyType =
    { typeStr = typeStr, typeName = typeName, valueType = valueType, keyType = keyType }

type Val = Bool | Int | Float | String | List Val | Dict Val Val | Maybe Val



sToType__ : String -> PolyEvalType
sToType__ typeStr =
    if not (String.contains "<" typeStr) then
        newPolyEvalType__ typeStr typeStr (PolyEvalTypeRef Nothing) (PolyEvalTypeRef Nothing)
    else
        let 
            idx = String.indexes "<" typeStr |> List.head |> Maybe.withDefault 0
            typeName = String.slice 0 idx typeStr
            otherStr = String.slice (idx + 1) (String.length typeStr - 1) typeStr
        in 
            if not (String.contains "," otherStr) then
                let 
                    valueType = sToType__ otherStr
                in 
                    newPolyEvalType__ typeStr typeName (PolyEvalTypeRef (Just valueType)) (PolyEvalTypeRef Nothing)
            else
                let 
                    idx1 = String.indexes "," otherStr |> List.head |> Maybe.withDefault 0
                    keyType = sToType__ (String.slice 0 idx1 otherStr)
                    valueType = sToType__ (String.slice (idx1 + 1) (String.length otherStr) otherStr)
                in 
                    newPolyEvalType__ typeStr typeName (PolyEvalTypeRef (Just valueType)) (PolyEvalTypeRef (Just keyType))

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
    let vs = Round.round 6 value |> String.toList |> List.foldr ( \c acc -> if c == '0' then acc else String.fromChar c ++ acc ) ""
        vs1 = if String.right 1 vs == "." then vs ++ "0" else vs
    in if vs1 == "-0.0" then "0.0" else vs1

byString__ : String -> String
byString__ value =
    "\"" ++ escapeString__ value ++ "\""

byList__ : List Val -> PolyEvalType -> String
byList__ value ty =
    let 
        valueType = case ty.valueType of
            PolyEvalTypeRef (Just t) -> t
            _ -> Debug.todo "Dict value type is not defined"  
        vStrs = List.map ( \v -> valToS__ v valueType ) value
    in "[" ++ String.join ", " vStrs ++ "]"

byUlist__ : List Val -> PolyEvalType -> String
byUlist__ value ty =
    let 
        valueType = case ty.valueType of
            PolyEvalTypeRef (Just t) -> t
            _ -> Debug.todo "Dict value type is not defined"  
        vStrs = List.map ( \v -> valToS__ v valueType ) value
    in "[" ++ String.join ", " (List.sort vStrs) ++ "]"

byDict__ : Dict Val Val -> PolyEvalType -> String
byDict__ value ty =
    let 
        keyType = case ty.keyType of
            PolyEvalTypeRef (Just t) -> t
            _ -> Debug.todo "Dict key type is not defined"
        valueType = case ty.valueType of
            PolyEvalTypeRef (Just t) -> t
            _ -> Debug.todo "Dict value type is not defined"    
        vStrs = Dict.toList value |> List.map ( \(key, val) -> valToS__ key keyType ++ "=>" ++ valToS__ val valueType )
    in "{" ++ String.join ", " (List.sort vStrs) ++ "}"

byOption__ : Maybe Val -> PolyEvalType -> String
byOption__ value ty =
    case value of
        Nothing -> "null"
        Just v -> 
            let
                valueType = case ty.valueType of
                    PolyEvalTypeRef (Just t) -> t
                    _ -> Debug.todo "Dict value type is not defined"  
            in valToS__ v valueType

valToS__ : Val -> PolyEvalType -> String
valToS__ value ty =
    let typeName = ty.typeName
    in case typeName of
        "bool" -> 
            case value of
                Bool -> byBool__ value
                _ -> Debug.todo "Type mismatch"
        "int" -> 
            case value of
                Int -> byInt__ value
                _ -> Debug.todo "Type mismatch"
        "double" ->
            case value of
                Float -> byDouble__ value
                _ -> Debug.todo "Type mismatch"
        "str" ->
            case value of
                String -> byString__ value
                _ -> Debug.todo "Type mismatch"
        "list" ->
            case value of
                List _ -> byList__ value ty
                _ -> Debug.todo "Type mismatch"
        "ulist" ->
            case value of
                List _ -> byUlist__ value ty
                _ -> Debug.todo "Type mismatch"
        "dict" ->
            case value of
                Dict _ _ -> byDict__ value ty
                _ -> Debug.todo "Type mismatch"
        "option" ->
            case value of
                Maybe _ -> byOption__ value ty
                _ -> Debug.todo "Type mismatch"
        _ -> Debug.todo ("Unknown type " ++ typeName)

stringify__ : Val -> String -> String
stringify__ value typeStr =
    valToS__ value(sToType__ typeStr) ++ ":" ++ typeStr

program : Process -> IO ()
program process =
    let tfs = stringify__ True "bool" ++ "\n"
    in File.writeContentsTo "stringify.out" tfs