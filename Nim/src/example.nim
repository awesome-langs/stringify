import strutils
import options
import algorithm
import sequtils
import sugar
import tables
import os

proc myStringToInt(s: string): int =
    result = parseInt(s)

proc myStringToDouble(s: string): float =
    result = parseFloat(s)

proc myIntToString(i: int): string =
    result = $i

proc myDoubleToString(d: float): string =
    result = d.formatFloat(ffDecimal, 6)

proc myBoolToString(b: bool): string =
    result = if b: "true" else: "false"

proc myIntToNullable(i: int): Option[int] =
    if i > 0:
        result = some(i)
    elif i < 0:
        result = some(-i)
    else:
        result = none(int)

proc myNullableToInt(i: Option[int]): int =
    result = if i.isSome(): i.get() else: -1

proc myListSorted(lst: seq[string]): seq[string] =
    result = lst.sorted()

proc myListSortedByLength(lst: seq[string]): seq[string] =
    result = lst.sortedByIt(it.len)

proc myListFilter(lst: seq[int]): seq[int] =
    result = lst.filterIt(it mod 3 == 0)

proc myListMap(lst: seq[int]): seq[int] =
    result = lst.mapIt(it * it)

proc myListReduce(lst: seq[int]): int =
    result = lst.foldl(a * 10 + b, 0)

proc myListOperations(lst: seq[int]): int =
    result = lst.filter(x => x mod 3 == 0)
        .map(x => x * x)
        .foldl(a * 10 + b, 0)

proc myListToDict(lst: seq[int]): Table[int, int] =
    result = toTable(lst.mapIt((it, it * it)))

proc myDictToList(dict: Table[int, int]): seq[int] =
    result = toSeq(dict.pairs()).sortedByIt(it[0]).mapIt(it[0] + it[1])

proc myPrintString(s: string): void =
    echo s

proc myPrintStringList(lst: seq[string]): void =
    for x in lst:
        stdout.write x & " "
    echo ""

proc myPrintIntList(lst: seq[int]): void =
    myPrintStringList(lst.mapIt(myIntToString(it)))

proc myPrintDict(dict: Table[int, int]): void =
    for k, v in dict.pairs:
        stdout.write myIntToString(k) & "->" & myIntToString(v) & " "
    echo ""

myPrintString("Hello, World!")
myPrintString(myIntToString(myStringToInt("123")))
myPrintString(myDoubleToString(myStringToDouble("123.456")))
myPrintString(myBoolToString(false))
myPrintString(myIntToString(myNullableToInt(myIntToNullable(18))))
myPrintString(myIntToString(myNullableToInt(myIntToNullable(-15))))
myPrintString(myIntToString(myNullableToInt(myIntToNullable(0))))
myPrintStringList(myListSorted(@["e", "dddd", "ccccc", "bb", "aaa"]))
myPrintStringList(myListSortedByLength(@["e", "dddd", "ccccc", "bb", "aaa"]))
myPrintString(myIntToString(myListReduce(myListMap(myListFilter(@[3, 12, 5, 8, 9, 15, 7, 17, 21, 11])))))
myPrintString(myIntToString(myListOperations(@[3, 12, 5, 8, 9, 15, 7, 17, 21, 11])))
myPrintDict(myListToDict(@[3, 1, 4, 2, 5, 9, 8, 6, 7, 0]))
myPrintIntList(myDictToList({3: 9, 1: 1, 4: 16, 2: 4, 5: 25, 9: 81, 8: 64, 6: 36, 7: 49, 0: 0}.toTable))