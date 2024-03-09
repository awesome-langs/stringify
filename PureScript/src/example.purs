module Example where

import Prelude 
import Effect (Effect)
import Effect.Console as Console
import Data.Int as Int
import Data.Number as Number
import Data.Number.Format (toStringWith, fixed)
import Data.String as String
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Foldable (for_, foldl)
import Data.FoldableWithIndex (forWithIndex_)

import Node.Encoding (Encoding(UTF8))
import Node.Process (stdout)
import Node.Stream (writeString)

myStringToInt :: String -> Int
myStringToInt s =
    case Int.fromString s of
        Just x -> x
        Nothing -> -1

myStringToDouble :: String -> Number
myStringToDouble s =
    case Number.fromString s of
        Just x -> x
        Nothing -> -1.0

myIntToString :: Int -> String
myIntToString i = 
    show i

myDoubleToString :: Number -> String
myDoubleToString d = 
    toStringWith (fixed 6) d

myBoolToString :: Boolean -> String
myBoolToString b = 
    if b then "true" else "false"

myIntToNullable :: Int -> Maybe Int
myIntToNullable i = 
    if i > 0 then Just i
    else if i < 0 then Just (-i)
    else Nothing

myNullableToInt :: Maybe Int -> Int
myNullableToInt i = 
    case i of
        Just x -> x
        Nothing -> -1

myListSorted :: Array String -> Array String
myListSorted lst = 
    Array.sort lst

myListSortedByLength :: Array String -> Array String
myListSortedByLength lst = 
    Array.sortBy (\x y -> (String.length x) `compare` (String.length y)) lst

myListFilter :: Array Int -> Array Int
myListFilter lst = 
    Array.filter (\x -> x `mod` 3 == 0) lst

myListMap :: Array Int -> Array Int
myListMap lst = 
    map (\x -> x * x) lst

myListReduce :: Array Int -> Int
myListReduce lst = 
    foldl (\acc x -> acc * 10 + x) 0 lst

myListOperations :: Array Int -> Int
myListOperations lst = 
    lst # Array.filter (\x -> x `mod` 3 == 0)
        # map (\x -> x * x)
        # foldl (\acc x -> acc * 10 + x) 0

myListToDict :: Array Int -> Map Int Int
myListToDict lst = 
    Map.fromFoldable (map (\x -> Tuple x (x * x)) lst)

myDictToList :: Map Int Int -> Array Int
myDictToList dict = 
    dict # (Map.toUnfoldable :: forall k v. Map k v -> Array (Tuple k v))
         # Array.sortBy (\(Tuple k1 _) (Tuple k2 _) -> k1 `compare` k2) # map (\(Tuple k v) -> k + v)

myPrintString :: String -> Effect Unit
myPrintString s = 
    Console.log s

myPrintStringList :: Array String -> Effect Unit
myPrintStringList lst = 
    do
    for_ lst (\x -> 
        void $ writeString stdout UTF8 (x <> " "))
    Console.log ""

myPrintIntList :: Array Int -> Effect Unit
myPrintIntList lst = 
    myPrintStringList $ map (\x -> myIntToString x) lst

myPrintDict :: Map Int Int -> Effect Unit
myPrintDict dict = 
    do
    forWithIndex_ dict (\k v -> 
        void $ writeString stdout UTF8 (myIntToString k <> "->" <> myIntToString v <> " "))
    Console.log ""

main :: Effect Unit
main = 
    do
    myPrintString "Hello, World!"
    myPrintString $ myIntToString $ myStringToInt "123"
    myPrintString $ myDoubleToString $ myStringToDouble "123.456"
    myPrintString $ myBoolToString false
    myPrintString $ myIntToString $ myNullableToInt $ myIntToNullable 18
    myPrintString $ myIntToString $ myNullableToInt $ myIntToNullable (-15)
    myPrintString $ myIntToString $ myNullableToInt $ myIntToNullable 0
    myPrintStringList $ myListSorted ["e", "dddd", "ccccc", "bb", "aaa"]
    myPrintStringList $ myListSortedByLength ["e", "dddd", "ccccc", "bb", "aaa"]
    myPrintString $ myIntToString $ myListReduce $ myListMap $ myListFilter [3, 12, 5, 8, 9, 15, 7, 17, 21, 11]
    myPrintString $ myIntToString $ myListOperations [3, 12, 5, 8, 9, 15, 7, 17, 21, 11]
    myPrintDict $ myListToDict [3, 1, 4, 2, 5, 9, 8, 6, 7, 0]
    myPrintIntList $ myDictToList $ Map.fromFoldable [Tuple 3 9, Tuple 1 1, Tuple 4 16, Tuple 2 4, Tuple 5 25, Tuple 9 81, Tuple 8 64, Tuple 6 36, Tuple 7 49, Tuple 0 0]