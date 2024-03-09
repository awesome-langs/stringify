function myStringToInt(s: string): number {
    return parseInt(s);
}

function myStringToDouble(s: string): number {
    return parseFloat(s);
}

function myIntToString(i: number): string {
    return i.toString();
}

function myDoubleToString(d: number): string {
    return d.toFixed(6);
}

function myBoolToString(b: boolean): string {
    return b ? "true" : "false";
}

function myIntToNullable(i: number): number | null {
    if (i > 0) {
        return i;
    } else if (i < 0) {
        return -i;
    } else {
        return null;
    }
}

function myNullableToInt(i: number | null): number {
    return i ?? -1;
}

function myListSorted(lst: string[]): string[] {
    return [...lst].sort();
}

function myListSortedByLength(lst: string[]): string[] {
    return [...lst].sort((a, b) => a.length - b.length);
}

function myListFilter(lst: number[]): number[] {
    return lst.filter(x => x % 3 === 0);
}

function myListMap(lst: number[]): number[] {
    return lst.map(x => x * x);
}

function myListReduce(lst: number[]): number {
    return lst.reduce((acc, x) => acc * 10 + x, 0);
}

function myListOperations(lst: number[]): number {
    return lst.filter(x => x % 3 === 0)
        .map(x => x * x)
        .reduce((acc, x) => acc * 10 + x, 0);
}

function myListToDict(lst: number[]): Map<number, number> {
    return new Map(lst.map(x => [x, x * x]));
}

function myDictToList(dict: Map<number, number>): number[] {
    return [...dict].sort((a, b) => a[0] - b[0]).map(x => x[0] + x[1]);
}

function myPrintString(s: string): void {
    console.log(s);
}

function myPrintStringList(lst: string[]): void {
    for (let x of lst) {
        process.stdout.write(x + " ");
    }
    console.log();
}

function myPrintIntList(lst: number[]): void {
    myPrintStringList(lst.map(x => myIntToString(x)));
}

function myPrintDict(dict: Map<number, number>): void {
    for (let [k, v] of dict) {
        process.stdout.write(myIntToString(k) + "->" + myIntToString(v) + " ");
    }
    console.log();
}

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
myPrintIntList(myDictToList(new Map([[3, 9], [1, 1], [4, 16], [2, 4], [5, 25], [9, 81], [8, 64], [6, 36], [7, 49], [0, 0]])));
