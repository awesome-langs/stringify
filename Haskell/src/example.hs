import Data.List
import Data.Function 
import Text.Printf
import Data.Map (Map)
import qualified Data.Map.Strict as Map

myStringToInt :: String -> Int
myStringToInt s = 
    read s :: Int

myStringToDouble :: String -> Double
myStringToDouble s = 
    read s :: Double

myIntToString :: Int -> String
myIntToString i = 
    show i

myDoubleToString :: Double -> String
myDoubleToString d = 
    printf "%.6f" d

myBoolToString :: Bool -> String
myBoolToString b = 
    if b then "true" else "false"

myIntToNullable :: Int -> Maybe Int
myIntToNullable i = 
    if i > 0 then Just i else if i < 0 then Just (-i) else Nothing

myNullableToInt :: Maybe Int -> Int
myNullableToInt i = 
    case i of
        Just x -> x
        Nothing -> -1

myListSorted :: [String] -> [String]
myListSorted lst = 
    sort lst

myListSortedByLength :: [String] -> [String]
myListSortedByLength lst = 
    sortBy (compare `on` length) lst

myListFilter :: [Int] -> [Int]
myListFilter lst = 
    filter (\x -> x `mod` 3 == 0) lst

myListMap :: [Int] -> [Int]
myListMap lst = 
    map (\x -> x * x) lst

myListReduce :: [Int] -> Int
myListReduce lst = 
    foldl' (\acc x -> acc * 10 + x) 0 lst

myListOperations :: [Int] -> Int
myListOperations lst = 
    lst & filter (\x -> x `mod` 3 == 0)
        & map (\x -> x * x)
        & foldl' (\acc x -> acc * 10 + x) 0

myListToDict :: [Int] -> Map Int Int
myListToDict lst = 
    lst & map (\x -> (x, x * x))
        & Map.fromList

myDictToList :: Map Int Int -> [Int]
myDictToList dict = 
    dict & Map.toList
        & sortBy (compare `on` fst)
        & map (\(k, v) -> k + v)

myPrintString :: String -> IO ()
myPrintString s = 
    putStrLn s

myPrintStringList :: [String] -> IO ()
myPrintStringList lst = 
    lst & foldr (\x acc -> 
        putStr (x ++ " ") >> acc) 
        (putStrLn "")

myPrintIntList :: [Int] -> IO ()
myPrintIntList lst = 
    myPrintStringList (map myIntToString lst)

myPrintDict :: Map Int Int -> IO ()
myPrintDict dict = 
    dict & Map.toList
        & foldr (\(k, v) acc -> 
        putStr (myIntToString k ++ "->" ++ myIntToString v ++ " ") >> acc) 
        (putStrLn "")

main :: IO ()
main = 
    myPrintString "Hello, World!"
    myPrintString (myIntToString (myStringToInt "123"))
    myPrintString (myDoubleToString (myStringToDouble "123.456"))
    myPrintString (myBoolToString False)
    myPrintString (myIntToString (myNullableToInt (myIntToNullable 18)))
    myPrintString (myIntToString (myNullableToInt (myIntToNullable (-15))))
    myPrintString (myIntToString (myNullableToInt (myIntToNullable 0)))
    myPrintStringList (myListSorted ["e", "dddd", "ccccc", "bb", "aaa"])
    myPrintStringList (myListSortedByLength ["e", "dddd", "ccccc", "bb", "aaa"])
    myPrintString (myIntToString (myListReduce (myListMap (myListFilter [3, 12, 5, 8, 9, 15, 7, 17, 21, 11]))))
    myPrintString (myIntToString (myListOperations [3, 12, 5, 8, 9, 15, 7, 17, 21, 11]))
    myPrintDict (myListToDict [3, 1, 4, 2, 5, 9, 8, 6, 7, 0])
    myPrintIntList (myDictToList (Map.fromList [(3, 9), (1, 1), (4, 16), (2, 4), (5, 25), (9, 81), (8, 64), (6, 36), (7, 49), (0, 0)]))