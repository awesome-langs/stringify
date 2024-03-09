import std.stdio;
import std.conv;
import std.algorithm;
import std.range;
import std.typecons;
import std.container;
import std.format;

int myStringToInt(string s) {
    return to!int(s);
}

double myStringToDouble(string s) {
    return to!double(s);
}

string myIntToString(int i) {
    return to!string(i);
}

string myDoubleToString(double d) {
    return format("%.6f", d);
}

string myBoolToString(bool b) {
    return b ? "true" : "false";
}

Nullable!int myIntToNullable(int i) {
    if (i > 0) {
        return Nullable!int(i);
    } else if (i < 0) {
        return Nullable!int(-i);
    } else {
        return Nullable!int();
    }
}

int myNullableToInt(Nullable!int i) {
    return i.get(-1);
}

string[] myListSorted(string[] lst) {
    return lst.array.sort.array;
}

string[] myListSortedByLength(string[] lst) {
    return lst.array.sort!((a, b) => a.length < b.length).array;
}

int[] myListFilter(int[] lst) {
    return lst.filter!(x => x % 3 == 0).array;
}

int[] myListMap(int[] lst) {
    return lst.map!(x => x * x).array;
}

int myListReduce(int[] lst) {
    return lst.reduce!((acc, x) => acc * 10 + x);
}

int myListOperations(int[] lst) {
    return lst.filter!(x => x % 3 == 0)
        .map!(x => x * x)
        .reduce!((acc, x) => acc * 10 + x);
}

int[int] myListToDict(int[] lst) {
    return lst.map!(x => tuple(x, x * x)).assocArray();
}

int[] myDictToList(int[int] dict) {
    return dict.byPair.array.sort!((a, b) => a[0] < b[0]).map!(x => x[0] + x[1]).array;
}

void myPrintString(string s) {
    writeln(s);
}

void myPrintStringList(string[] lst) {
    foreach (x; lst) {
        write(x ~ " ");
    }
    writeln();
}

void myPrintIntList(int[] lst) {
    myPrintStringList(lst.map!(x => myIntToString(x)).array);
}

void myPrintDict(int[int] dict) {
    foreach (k, v; dict) {
        write(myIntToString(k) ~ "->" ~ myIntToString(v) ~ " ");
    }
    writeln();
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
    myPrintIntList(myDictToList([3: 9, 1: 1, 4: 16, 2: 4, 5: 25, 9: 81, 8: 64, 6: 36, 7: 49, 0: 0]));
}
