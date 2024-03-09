using Lambda;
import de.polygonal.Printf;

class Example {
    public static function myStringToInt(s: String): Int {
        return Std.parseInt(s);
    }

    public static function myStringToDouble(s: String): Float {
        return Std.parseFloat(s);
    }

    public static function myIntToString(i: Int): String {
        return Std.string(i);
    }

    public static function myDoubleToString(d: Float): String {
        return Printf.format("%.6f", [d]);
    }

    public static function myBoolToString(b: Bool): String {
        return b ? "true" : "false";
    }

    public static function myIntToNullable(i: Int): Null<Int> {
        if (i > 0) {
            return i;
        } else if (i < 0) {
            return -i;
        } else {
            return null;
        }
    }

    public static function myNullableToInt(i: Null<Int>): Int {
        return i != null ? i : -1;
    }

    public static function myListSorted(lst: Array<String>): Array<String> {
        var tmp = lst.copy();
        tmp.sort(Reflect.compare);
        return tmp;
    }

    public static function myListSortedByLength(lst: Array<String>): Array<String> {
        var tmp = lst.copy();
        tmp.sort((a, b) -> a.length - b.length);
        return tmp;
    }

    public static function myListFilter(lst: Array<Int>): Array<Int> {
        return lst.filter(x -> x % 3 == 0);
    }

    public static function myListMap(lst: Array<Int>): Array<Int> {
        return lst.map(x -> x * x);
    }

    public static function myListReduce(lst: Array<Int>): Int {
        return lst.fold((x, acc) -> acc * 10 + x, 0);
    }

    public static function myListOperations(lst: Array<Int>): Int {
        return lst.filter(x -> x % 3 == 0)
            .map(x -> x * x)
            .fold((x, acc) -> acc * 10 + x, 0);
    }

    public static function myListToDict(lst: Array<Int>): Map<Int, Int> {
        var dict = new Map<Int, Int>();
        for (i in lst) {
            dict.set(i, i * i);
        }
        return dict;
    }

    public static function myDictToList(dict: Map<Int, Int>): Array<Int> {
        var keys = [];
        for (k in dict.keys()) {
            keys.push(k);
        }
        keys.sort(Reflect.compare);
        var tmp = [];
        for (k in keys) {
            tmp.push(k + dict.get(k));
        }
        return tmp;
    }

    public static function myPrintString(s: String): Void {
        Sys.println(s);
    }

    public static function myPrintStringList(lst: Array<String>): Void {
        for (x in lst) {
            Sys.print(x + " ");
        }
        Sys.println("");
    }

    public static function myPrintIntList(lst: Array<Int>): Void {
        myPrintStringList(lst.map(x -> myIntToString(x)));
    }

    public static function myPrintDict(dict: Map<Int, Int>): Void {
        for (k => v in dict) {
            Sys.print(myIntToString(k) + "->" + myIntToString(v) + " ");
        }
        Sys.println("");
    }

    public static function main(): Void {
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
        myPrintIntList(myDictToList([3 => 9, 1 => 1, 4 => 16, 2 => 4, 5 => 25, 9 => 81, 8 => 64, 6 => 36, 7 => 49, 0 => 0]));
    }
}