module Writable = NodeJs.Stream.Writable
module Process = NodeJs.Process
module Buffer = NodeJs.Buffer

let myStringToInt = (s: string): int => {
    Int.fromString(s)->Option.getOr(-1)
}

let myStringToDouble = (s: string): float => {
    Float.fromString(s)->Option.getOr(-1.0)
}

let myIntToString = (i: int): string => {
    Int.toString(i)
}

let myDoubleToString = (d: float): string => {
    Float.toFixedWithPrecision(d, ~digits=6)
}

let myBoolToString = (b: bool): string => {
    b ? "true" : "false"
}

let myIntToNullable = (i: int): option<int> => {
    if (i > 0) {
        Some(i)
    } else if (i < 0) {
        Some(-i)
    } else {
        None
    }
}

let myNullableToInt = (i: option<int>): int => {
    i->Option.getOr(-1)
}

let myListSorted = (lst: array<string>): array<string> => {
    Array.toSorted(lst, String.compare)
}

let myListSortedByLength = (lst: array<string>): array<string> => {
    Array.toSorted(lst, (a, b) => Int.compare(String.length(a), String.length(b)))
}

let myListFilter = (lst: array<int>): array<int> => {
    Array.filter(lst, x => x->mod(3) == 0)
}

let myListMap = (lst: array<int>): array<int> => {
    Array.map(lst, x => x * x)
}

let myListReduce = (lst: array<int>): int => {
    Array.reduce(lst, 0, (acc, x) => acc * 10 + x)
}

let myListOperations = (lst: array<int>): int => {
    lst->Array.filter(x => x->mod(3) == 0)
      ->Array.map(x => x * x)
      ->Array.reduce(0, (acc, x) => acc * 10 + x)
}

let myListToDict = (lst: array<int>): Map.t<int, int> => {
    lst->Array.map(x => (x, x * x))->Map.fromArray
}

let myDictToList = (dict: Map.t<int, int>): array<int> => {
    dict->Map.entries->Iterator.toArray
       ->Array.toSorted(((a, _), (b, _)) => Int.compare(a, b))
       ->Array.map(((k, v)) => k + v)
}

let myPrintString = (s: string): unit => {
    Console.log(s)
}

let myPrintStringList = (lst: array<string>): unit => {
    lst->Array.forEach(x =>
        Process.process->Process.stdout->
        Writable.write(Buffer.fromString(x ++ " "))->ignore
    )
    Console.log("")
}

let myPrintIntList = (lst: array<int>): unit => {
    lst->Array.map(x => myIntToString(x))->myPrintStringList
}

let myPrintDict = (dict: Map.t<int, int>): unit => {
    dict->Map.forEachWithKey((v, k) => 
        Process.process->Process.stdout->
        Writable.write(Buffer.fromString(myIntToString(k) ++ "->" ++ myIntToString(v) ++ " "))->ignore
        )
    Console.log("")
}

myPrintString("Hello, World!")
myPrintString(myIntToString(myStringToInt("123")))
myPrintString(myDoubleToString(myStringToDouble("123.456")))
myPrintString(myBoolToString(false))
myPrintString(myIntToString(myNullableToInt(myIntToNullable(18))))
myPrintString(myIntToString(myNullableToInt(myIntToNullable(-15))))
myPrintString(myIntToString(myNullableToInt(myIntToNullable(0))))
myPrintStringList(myListSorted(["e", "dddd", "ccccc", "bb", "aaa"]))
myPrintStringList(myListSortedByLength(["e", "dddd", "ccccc", "bb", "aaa"]))
myPrintString(myIntToString(myListReduce(myListMap(myListFilter([3, 12, 5, 8, 9, 15, 7, 17, 21, 11])))))
myPrintString(myIntToString(myListOperations([3, 12, 5, 8, 9, 15, 7, 17, 21, 11])))
myPrintDict(myListToDict([3, 1, 4, 2, 5, 9, 8, 6, 7, 0]))
myPrintIntList(myDictToList(Map.fromArray([(3, 9), (1, 1), (4, 16), (2, 4), (5, 25), (9, 81), (8, 64), (6, 36), (7, 49), (0, 0)])))