import Foundation

func myStringToInt(_ s: String) -> Int {
    return Int(s)!
}

func myStringToDouble(_ s: String) -> Double {
    return Double(s)!
}

func myIntToString(_ i: Int) -> String {
    return String(i)
}

func myDoubleToString(_ d: Double) -> String {
    return String(format: "%.6f", d)
}

func myBoolToString(_ b: Bool) -> String {
    return b ? "true" : "false"
}

func myIntToNullable(_ i: Int) -> Int? {
    if i > 0 {
        return i
    } else if i < 0 {
        return -i
    } else {
        return nil
    }
}

func myNullableToInt(_ i: Int?) -> Int {
    return i ?? -1
}

func myListSorted(_ lst: [String]) -> [String] {
    return lst.sorted()
}

func myListSortedByLength(_ lst: [String]) -> [String] {
    return lst.sorted { $0.count < $1.count }
}

func myListFilter(_ lst: [Int]) -> [Int] {
    return lst.filter { $0 % 3 == 0 }
}

func myListMap(_ lst: [Int]) -> [Int] {
    return lst.map { $0 * $0 }
}

func myListReduce(_ lst: [Int]) -> Int {
    return lst.reduce(0) { $0 * 10 + $1 }
}

func myListOperations(_ lst: [Int]) -> Int {
    return lst.filter { (x) in x % 3 == 0 }
        .map { (x) in x * x }
        .reduce(0) { (acc, x) in acc * 10 + x }
}

func myListToDict(_ lst: [Int]) -> [Int: Int] {
    return Dictionary(uniqueKeysWithValues: lst.map { ($0, $0 * $0) })
}

func myDictToList(_ dict: [Int: Int]) -> [Int] {
    return dict.sorted { $0.key < $1.key }.map { $0.key + $0.value }
}

func myPrintString(_ s: String) {
    print(s)
}

func myPrintStringList(_ lst: [String]) {
    for x in lst {
        print(x + " ", terminator: "")
    }
    print()
}

func myPrintIntList(_ lst: [Int]) {
    myPrintStringList(lst.map { myIntToString($0) })
}

func myPrintDict(_ dict: [Int: Int]) {
    for (k, v) in dict {
        print(myIntToString(k) + "->" + myIntToString(v) + " ", terminator: "")
    }
    print()
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
myPrintIntList(myDictToList([3: 9, 1: 1, 4: 16, 2: 4, 5: 25, 9: 81, 8: 64, 6: 36, 7: 49, 0: 0]))