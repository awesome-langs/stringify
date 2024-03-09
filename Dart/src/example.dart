import "dart:io";

int myStringToInt(String s) {
    return int.parse(s);
}

double myStringToDouble(String s) {
    return double.parse(s);
}

String myIntToString(int i) {
    return i.toString();
}

String myDoubleToString(double d) {
    return d.toStringAsFixed(6);
}

String myBoolToString(bool b) {
    return b ? "true" : "false";
}

int? myIntToNullable(int i) {
    if (i > 0) {
        return i;
    } else if (i < 0) {
        return -i;
    } else {
        return null;
    }
}

int myNullableToInt(int? i) {
    return i ?? -1;
}

List<String> myListSorted(List<String> lst) {
    return [...lst]..sort();
}

List<String> myListSortedByLength(List<String> lst) {
    return [...lst]..sort((a, b) => a.length - b.length);
}

List<int> myListFilter(List<int> lst) {
    return lst.where((x) => x % 3 == 0).toList();
}

List<int> myListMap(List<int> lst) {
    return lst.map((x) => x * x).toList();
}

int myListReduce(List<int> lst) {
    return lst.fold(0, (acc, x) => acc * 10 + x);
}

int myListOperations(List<int> lst) {
    return lst.where((x) => x % 3 == 0)
        .map((x) => x * x)
        .fold(0, (acc, x) => acc * 10 + x);
}

Map<int, int> myListToDict(List<int> lst) {
    return Map.fromIterable(lst, key: (x) => x, value: (x) => x * x);
}

List<int> myDictToList(Map<int, int> lst) {
    return (lst.entries.toList()..sort((a, b) => a.key - b.key)).map((x) => x.key +x.value).toList();
}

void myPrintString(String s) {
    print(s);
}

void myPrintStringList(List<String> lst) {
    for (var x in lst) {
        stdout.write(x + " ");
    }
    print("");
}

void myPrintIntList(List<int> lst) {
    myPrintStringList(lst.map((x) => myIntToString(x)).toList());
}

void myPrintDict(Map<int, int> dict) {
    for (var e in dict.entries) {
        stdout.write(myIntToString(e.key) + "->" + myIntToString(e.value) + " ");
    }
    print("");
}

void main() {
    myPrintString("Hello, World!");
    myPrintString(myIntToString(myStringToInt("123")));
    myPrintString(myDoubleToString(myStringToDouble("123.456")));
    myPrintString(myBoolToString(false));
    myPrintString(myIntToString(myNullableToInt(myIntToNullable(18))));
    myPrintString(myIntToString(myNullableToInt(myIntToNullable(-15))));
    myPrintString(myIntToString(myNullableToInt(myIntToNullable(0))));
    myPrintStringList(myListSorted(["e", "dddd", "ccccc", "bb", "aaa"]));
    myPrintStringList(myListSortedByLength(["e", "dddd", "ccccc", "bb", "aaa"]));
    myPrintString(myIntToString(myListReduce(myListMap(myListFilter([3, 12, 5, 8, 9, 15, 7, 17, 21, 11])))));
    myPrintString(myIntToString(myListOperations([3, 12, 5, 8, 9, 15, 7, 17, 21, 11])));
    myPrintDict(myListToDict([3, 1, 4, 2, 5, 9, 8, 6, 7, 0]));
    myPrintIntList(myDictToList({3: 9, 1: 1, 4: 16, 2: 4, 5: 25, 9: 81, 8: 64, 6: 36, 7: 49, 0: 0}));
}