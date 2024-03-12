{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}

import Data.List
import Data.Function 
import Data.Maybe
import Data.Map (Map)
import Text.Printf
import qualified Data.Map.Strict as Map

data PolyEvalType = PolyEvalType { typeStr :: String, typeName :: String, valueType :: Maybe PolyEvalType, keyType :: Maybe PolyEvalType }

class ValToS__ a where  
    valToS__ :: a -> PolyEvalType -> String
    valToS__ _ _ = error "Wrong Type"

sToType__ :: String -> PolyEvalType
sToType__ typeStr = 
    if "<" `isInfixOf` typeStr then
        let idx = head $ elemIndices '<' typeStr
            typeName = take idx typeStr
            otherStr = take (length typeStr - idx - 2) $ drop (idx + 1) typeStr
        in if "," `isInfixOf` otherStr then
            let idx = head $ elemIndices ',' otherStr
                keyType = sToType__ $ take idx otherStr
                valueType = sToType__ $ drop (idx + 1) otherStr
            in PolyEvalType typeStr typeName (Just valueType) (Just keyType)
        else
            let valueType = sToType__ otherStr
            in PolyEvalType typeStr typeName (Just valueType) Nothing
    else
        PolyEvalType typeStr typeStr Nothing Nothing

escapeString__ :: String -> String
escapeString__ s = 
    let escapeChar c = case c of
            '\\' -> "\\\\"
            '\"' -> "\\\""
            '\n' -> "\\n"
            '\t' -> "\\t"
            '\r' -> "\\r"
            _ -> [c]
    in concatMap escapeChar s

byBool__ :: Bool -> String
byBool__ value = 
    if value then "true" else "false"

byInt__ :: Int -> String
byInt__ value = 
    show value

byDouble__ :: Double -> String
byDouble__ value = 
    let vs = printf "%.6f" value
    in if isSuffixOf "0" vs then
        let vs' = reverse $ dropWhile (== '0') $ reverse vs
        in if isSuffixOf "." vs' then vs' ++ "0" else vs'
    else if vs == "-0.0" then "0.0" else vs

byString__ :: String -> String
byString__ value = 
    "\"" ++ escapeString__ value ++ "\""

byList__ :: (ValToS__ a) => [a] -> PolyEvalType -> String
byList__ value ty =
    let vStrs = value & map (\v -> valToS__ v (fromJust $ valueType ty)) 
    in "[" ++ intercalate ", " vStrs ++ "]"

byUList__ :: (ValToS__ a) => [a] -> PolyEvalType -> String
byUList__ value ty = 
    let vStrs = value & map (\v -> valToS__ v (fromJust $ valueType ty)) 
    in "[" ++ intercalate ", " (sort vStrs) ++ "]"

byDict__ :: (ValToS__ a, ValToS__ b) => (Map a b) -> PolyEvalType -> String
byDict__ value ty = 
    let vStrs = Map.toList value & map (\(key, val) -> (valToS__ key (fromJust $ keyType ty)) ++ "=>" ++ (valToS__ val (fromJust $ valueType ty)))
    in "{" ++ intercalate ", " (sort vStrs) ++ "}"

byOption__ :: (ValToS__ a) => Maybe a -> PolyEvalType -> String
byOption__ value ty = 
    if isNothing value then "null" else valToS__ (fromJust value) (fromJust $ valueType ty) 

instance ValToS__ Bool where
    valToS__ value ty = 
        if (typeName ty) == "bool" then byBool__ value else error "Type mismatch"

instance ValToS__ Int where
    valToS__ value ty = 
        if (typeName ty) == "int" then byInt__ value else error "Type mismatch"

instance ValToS__ Double where
    valToS__ value ty = 
        if (typeName ty) == "double" then byDouble__ value else error "Type mismatch"

instance {-# OVERLAPPING #-} ValToS__ String where
    valToS__ value ty = 
        if (typeName ty) == "str" then byString__ value else error "Type mismatch"

instance (ValToS__ a) => ValToS__ [a] where
    valToS__ value ty = 
        if (typeName ty) == "list" then byList__ value ty 
        else if (typeName ty) == "ulist" then byUList__ value ty 
        else error "Type mismatch"

instance (ValToS__ a, ValToS__ b) => ValToS__ (Map a b) where
    valToS__ value ty = 
        if (typeName ty) == "dict" then byDict__ value ty else error "Type mismatch"

instance (ValToS__ a) => ValToS__ (Maybe a) where
    valToS__ value ty = 
        if (typeName ty) == "option" then byOption__ value ty else error "Type mismatch"

stringify__ :: (ValToS__ a) => a -> String -> String
stringify__ value typeStr = 
    valToS__ value (sToType__ typeStr) ++ ":" ++ typeStr

main :: IO ()
main = do
    let tfs = stringify__ True "bool" ++ "\n" 
            ++ stringify__ (3 :: Int) "int" ++ "\n" 
            ++ stringify__ (3.141592653 :: Double) "double" ++ "\n" 
            ++ stringify__ (3.0 :: Double) "double" ++ "\n" 
            ++ stringify__ "Hello, World!" "str" ++ "\n" 
            ++ stringify__ "!@#$%^&*()\\\"\n\t" "str" ++ "\n" 
            ++ stringify__ [1 :: Int, 2 :: Int, 3 :: Int] "list<int>" ++ "\n" 
            ++ stringify__ [True, False, True] "list<bool>" ++ "\n" 
            ++ stringify__ [3 :: Int, 2 :: Int, 1 :: Int] "ulist<int>" ++ "\n" 
            ++ stringify__ (Map.fromList [(1 :: Int, "one"), (2 :: Int, "two")]) "dict<int,str>" ++ "\n" 
            ++ stringify__ (Map.fromList [("one", [1 :: Int, 2 :: Int, 3 :: Int]), ("two", [4 :: Int, 5 :: Int, 6 :: Int])]) "dict<str,list<int>>" ++ "\n" 
            ++ stringify__ (Nothing :: Maybe Bool) "option<int>" ++ "\n" 
            ++ stringify__ (Just (3 :: Int)) "option<int>" ++ "\n"
    writeFile "stringify.out" tfs