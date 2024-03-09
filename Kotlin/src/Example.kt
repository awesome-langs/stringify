fun myStringToInt(s: String): Int {
    return s.toInt()
}

fun myStringToDouble(s: String): Double {
    return s.toDouble()
}

fun myIntToString(i: Int): String {
    return i.toString()
}

fun myDoubleToString(d: Double): String {
    return String.format("%.6f", d)
}

fun myBoolToString(b: Boolean): String {
    return if (b) "true" else "false"
}

fun myIntToNullable(i: Int): Int? {
    if (i > 0) {
        return i
    } else if (i < 0) {
        return -i
    } else {
        return null
    }
}

fun myNullableToInt(i: Int?): Int {
    return i ?: -1
}

fun myListSorted(lst: List<String>): List<String> {
    return lst.sorted()
}

fun myListSortedByLength(lst: List<String>): List<String> {
    return lst.sortedBy { it.length }
}

fun myListFilter(lst: List<Int>): List<Int> {
    return lst.filter { it % 3 == 0 }
}

fun myListMap(lst: List<Int>): List<Int> {
    return lst.map { it * it }
}

fun myListReduce(lst: List<Int>): Int {
    return lst.fold(0) { acc, x -> acc * 10 + x }
}

fun myListOperations(lst: List<Int>): Int {
    return lst.filter { x -> x % 3 == 0 }
        .map { x -> x * x }
        .fold(0) { acc, x -> acc * 10 + x }
}

fun myListToDict(lst: List<Int>): Map<Int, Int> {
    return lst.associateWith { it * it }
}

fun myDictToList(lst: Map<Int, Int>): List<Int> {
    return lst.toList().sortedBy { it.first }.map { it.first + it.second }
}

fun myPrintString(s: String) {
    println(s)
}

fun myPrintStringList(lst: List<String>) {
    for (x in lst) {
        print(x + " ")
    }
    println()
}

fun myPrintIntList(lst: List<Int>) {
    myPrintStringList(lst.map { myIntToString(it) })
}

fun myPrintDict(dict: Map<Int, Int>) {
    for ((k, v) in dict) {
        print(myIntToString(k) + "->" + myIntToString(v) + " ")
    }
    println()
}

fun main() {
    myPrintString("Hello, World!")
    myPrintString(myIntToString(myStringToInt("123")))
    myPrintString(myDoubleToString(myStringToDouble("123.456")))
    myPrintString(myBoolToString(false))
    myPrintString(myIntToString(myNullableToInt(myIntToNullable(18))))
    myPrintString(myIntToString(myNullableToInt(myIntToNullable(-15))))
    myPrintString(myIntToString(myNullableToInt(myIntToNullable(0))))
    myPrintStringList(myListSorted(listOf("e", "dddd", "ccccc", "bb", "aaa")))
    myPrintStringList(myListSortedByLength(listOf("e", "dddd", "ccccc", "bb", "aaa")))
    myPrintString(myIntToString(myListReduce(myListMap(myListFilter(listOf(3, 12, 5, 8, 9, 15, 7, 17, 21, 11))))))
    myPrintString(myIntToString(myListOperations(listOf(3, 12, 5, 8, 9, 15, 7, 17, 21, 11))))
    myPrintDict(myListToDict(listOf(3, 1, 4, 2, 5, 9, 8, 6, 7, 0)))
    myPrintIntList(myDictToList(mapOf(3 to 9, 1 to 1, 4 to 16, 2 to 4, 5 to 25, 9 to 81, 8 to 64, 6 to 36, 7 to 49, 0 to 0)))
}