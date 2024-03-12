module Example where

import Prelude 
import Data.Array as Array
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Number.Format (toStringWith, fixed)
import Effect (Effect)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..))
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import Partial.Unsafe (unsafeCrashWith)

fromJust__ :: forall a. Maybe a -> a
fromJust__ x = case x of
  Nothing -> unsafeCrashWith "unsafeCrash"
  Just a -> a

newtype PolyEvalType = PolyEvalType
    { typeStr :: String, typeName :: String, valueType :: Maybe PolyEvalType, keyType :: Maybe PolyEvalType }

newPolyEvalType__ :: String -> String -> Maybe PolyEvalType -> Maybe PolyEvalType -> PolyEvalType
newPolyEvalType__ typeStr typeName valueType keyType =
    PolyEvalType { typeStr : typeStr, typeName, valueType, keyType }
    
getTypeStr__ :: PolyEvalType -> String
getTypeStr__ (PolyEvalType { typeStr }) = typeStr

getTypeName__ :: PolyEvalType -> String
getTypeName__ (PolyEvalType { typeName }) = typeName

getValueType__ :: PolyEvalType -> Maybe PolyEvalType
getValueType__ (PolyEvalType { valueType }) = valueType

getKeyType__ :: PolyEvalType -> Maybe PolyEvalType
getKeyType__ (PolyEvalType { keyType }) = keyType


sToType__ :: String -> PolyEvalType
sToType__ typeStr = 
    if String.contains (String.Pattern "<") typeStr then
        let idx = fromJust__ (String.indexOf (String.Pattern "<") typeStr)
            typeName = CodeUnits.slice 0 idx typeStr
            otherStr = CodeUnits.slice (idx + 1) (-1) typeStr
        in if String.contains (String.Pattern ",") otherStr then
            let idx = fromJust__ (String.indexOf (String.Pattern ",") otherStr)
                keyType = sToType__ $ CodeUnits.slice 0 idx otherStr
                valueType = sToType__ $ CodeUnits.slice (idx + 1) (String.length otherStr) otherStr
            in newPolyEvalType__ typeStr typeName (Just valueType) (Just keyType)
        else
            let valueType = sToType__ otherStr
            in newPolyEvalType__ typeStr typeName (Just valueType) Nothing
    else
        newPolyEvalType__ typeStr typeStr Nothing Nothing

class ValToS__ a where  
    valToS__ :: a -> PolyEvalType -> String

escapeString__ :: String -> String
escapeString__ s = 
    s # CodeUnits.toCharArray # map (\c -> 
            if c == '\\' then "\\\\"
            else if c == '\"' then "\\\""
            else if c == '\n' then "\\n"
            else if c == '\t' then "\\t"
            else if c == '\t' then "\\t"
            else if c == '\r' then "\\r"
            else CodeUnits.fromCharArray [c]
        ) # String.joinWith ""

byBool__ :: Boolean -> String
byBool__ value = 
    if value then "true" else "false"

byInt__ :: Int -> String
byInt__ value = 
    show value

byDouble__ :: Number -> String
byDouble__ value = 
    toStringWith (fixed 6) value

byString__ :: String -> String
byString__ value = 
    "\"" <> escapeString__ value <> "\""

byList__ :: forall a. (ValToS__ a) => Array a -> PolyEvalType -> String
byList__ value ty =
    let vStrs = value # map (\v -> valToS__ v (fromJust__ (getValueType__ ty))) 
    in "[" <> String.joinWith ", " vStrs <> "]"

byUList__ :: forall a. (ValToS__ a) => Array a -> PolyEvalType -> String
byUList__ value ty = 
    let vStrs = value # map (\v -> valToS__ v (fromJust__ (getValueType__ ty))) 
    in "[" <> String.joinWith ", " (Array.sort vStrs) <> "]"

byDict__ :: forall a. (ValToS__ a) => forall b. (ValToS__ b) => Map a b -> PolyEvalType -> String
byDict__ value ty = 
    let vStrs = (Map.toUnfoldable :: forall k v. Map k v -> Array (Tuple k v)) value # map (\(Tuple key val) -> (valToS__ key (fromJust__ (getKeyType__ ty))) <> "=>" <> (valToS__ val (fromJust__ (getValueType__ ty))))
    in "{" <> String.joinWith ", " (Array.sort vStrs) <> "}"

byOption__ :: forall a. (ValToS__ a) => Maybe a -> PolyEvalType -> String
byOption__ value ty = 
    if isNothing value then "null" else valToS__ (fromJust__ value) (fromJust__ (getValueType__ ty))

instance valToSBool :: ValToS__ Boolean where
    valToS__ value ty = 
        if ((getTypeName__ ty)) == "bool" then byBool__ value else unsafeCrashWith "Type mismatch"
    
instance valToSInt :: ValToS__ Int where
    valToS__ value ty = 
        if (getTypeName__ ty) == "int" then byInt__ value else unsafeCrashWith "Type mismatch"

instance valToSDouble :: ValToS__ Number where
    valToS__ value ty = 
        if (getTypeName__ ty) == "double" then byDouble__ value else unsafeCrashWith "Type mismatch"
    
instance valToSString :: ValToS__ String where
    valToS__ value ty = 
        if (getTypeName__ ty) == "str" then byString__ value else unsafeCrashWith "Type mismatch"

instance valToSList :: (ValToS__ a) => ValToS__ (Array a) where
    valToS__ value ty = 
        if (getTypeName__ ty) == "list" then byList__ value ty 
        else if (getTypeName__ ty) == "ulist" then byUList__ value ty 
        else unsafeCrashWith "Type mismatch"

instance valToSDict :: (ValToS__ a, ValToS__ b) => ValToS__ (Map a b) where
    valToS__ value ty = 
        if (getTypeName__ ty) == "dict" then byDict__ value ty else unsafeCrashWith "Type mismatch"

instance valToSOption :: (ValToS__ a) => ValToS__ (Maybe a) where
    valToS__ value ty = 
        if (getTypeName__ ty) == "option" then byOption__ value ty else unsafeCrashWith "Type mismatch"

stringify__ :: forall a. (ValToS__ a) => a -> String -> String
stringify__ value typeStr = 
    valToS__ value (sToType__ typeStr) <> ":" <> typeStr

main :: Effect Unit
main = 
    let tfs = stringify__ true "bool" <> "\n" 
            <> stringify__ 3 "int" <> "\n" 
            <> stringify__ 3.141592653 "double" <> "\n" 
            <> stringify__ 3.0 "double" <> "\n" 
            <> stringify__ "Hello, World!" "str" <> "\n" 
            <> stringify__ "!@#$%^&*()\\\"\n\t" "str" <> "\n" 
            <> stringify__ [1, 2, 3] "list<int>" <> "\n" 
            <> stringify__ [true, false, true] "list<bool>" <> "\n" 
            <> stringify__ [3, 2, 1] "ulist<int>" <> "\n" 
            <> stringify__ (Map.fromFoldable [Tuple 1 "one", Tuple 2 "two"]) "dict<int,str>" <> "\n" 
            <> stringify__ (Map.fromFoldable [Tuple "one" [1, 2, 3], Tuple "two" [4, 5, 6]]) "dict<str,list<int>>" <> "\n" 
            <> stringify__ (Nothing :: Maybe Boolean) "option<int>" <> "\n" 
            <> stringify__ (Just 3) "option<int>" <> "\n"
    in writeTextFile UTF8 "stringify.out" tfs