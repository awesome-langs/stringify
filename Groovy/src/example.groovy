int myStringToInt(String s) {
    return s.toInteger()
}

double myStringToDouble(String s) {
    return s.toDouble()
}

String myIntToString(int i) {
    return i.toString()
}

String myDoubleToString(double d) {
    return String.format("%.6f", d)
}

String myBoolToString(boolean b) {
    return b ? "true" : "false"
}

Integer myIntToNullable(int i) {
    if (i > 0) {
        return i
    } else if (i < 0) {
        return -i
    } else {
        return null
    }
}

int myNullableToInt(Integer i) {
    return i ?: -1
}

List<String> myListSorted(List<String> lst) {
    return lst.sort()
}

List<String> myListSortedByLength(List<String> lst) {
    return lst.sort { it.length() }
}

List<Integer> myListFilter(List<Integer> lst) {
    return lst.findAll { it % 3 == 0 }
}

List<Integer> myListMap(List<Integer> lst) {
    return lst.collect { it * it }
}

int myListReduce(List<Integer> lst) {
    return lst.inject(0) { acc, x -> acc * 10 + x }
}

int myListOperations(List<Integer> lst) {
    return lst.findAll { x -> x % 3 == 0 }
        .collect { x -> x * x }
        .inject(0) { acc, x -> acc * 10 + x }
}

Map<Integer, Integer> myListToDict(List<Integer> lst) {
    return lst.collectEntries { [it, it * it] }
}

List<Integer> myDictToList(Map<Integer, Integer> dict) {
    return dict.sort { it.key }.collect { it.key + it.value }
}

void myPrintString(String s) {
    println(s)
}

void myPrintStringList(List<String> lst) {
    for (x in lst) {
        print(x + " ")
    }
    println()
}

void myPrintIntList(List<Integer> lst) {
    myPrintStringList(lst.collect { myIntToString(it) })
}

void myPrintDict(Map<Integer, Integer> dict) {
    for (e in dict) {
        print(myIntToString(e.key) + "->" + myIntToString(e.value) + " ")
    }
    println()
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
