package main

import (
	"fmt"
	"strconv"
	"slices"
    "cmp"
)

func MyStringToInt(s string) int {
    i, _ := strconv.Atoi(s)
    return i
}

func MyStringToDouble(s string) float64 {
    d, _ := strconv.ParseFloat(s, 64)
    return d
}

func MyIntToString(i int) string {
    return strconv.Itoa(i)
}

func MyDoubleToString(d float64) string {
    return fmt.Sprintf("%.6f", d)
}

func MyBoolToString(b bool) string {
    if b {
        return "true"
    } else {
        return "false"
    }
}

func MyIntToNullable(i int) *int {
    tmp := new(int)
    if i > 0 {
        *tmp = i
    } else if i < 0 {
        *tmp = -i
    } else {
        tmp = nil
    }
    return tmp
}

func MyNullableToInt(i *int) int {
    if i == nil {
        return -1
    } else {
        return *i
    }
}

func MyListSorted(lst []string) []string {
    tmp := slices.Clone(lst)
    slices.Sort(tmp)
    return tmp
}

func MyListSortedByLength(lst []string) []string {
    tmp := slices.Clone(lst)
    slices.SortFunc(tmp, func(a, b string) int {
        return cmp.Compare(len(a), len(b))
    })
    return tmp
}

func MyListFilter(lst []int) []int {
    tmp := []int{}
    for _, x := range lst {
        if x % 3 == 0 {
            tmp = append(tmp, x)
        }
    }
    return tmp
}

func MyListMap(lst []int) []int {
    tmp := []int{}
    for _, x := range lst {
        tmp = append(tmp, x * x)
    }
    return tmp
}

func MyListReduce(lst []int) int {
    tmp := 0
    for _, x := range lst {
        tmp = tmp * 10 + x
    }
    return tmp
}

func MyListOperations(lst []int) int {
    tmp := 0
    for _, x := range lst {
        if x % 3 == 0 {
            tmp = tmp * 10 + x * x
        }
    }
    return tmp
}

func MyListToDict(lst []int) map[int]int {
    tmp := make(map[int]int)
    for _, x := range lst {
        tmp[x] = x * x
    }
    return tmp
}

func MyDictToList(dict map[int]int) []int {
    keys := []int{}
    for k, _ := range dict {
        keys = append(keys, k)
    }
    slices.Sort(keys)
    tmp := []int{}
    for _, k := range keys {
        tmp = append(tmp, k + dict[k])
    }
    return tmp
}

func MyPrintString(s string) {
    fmt.Println(s)
}

func MyPrintStringList(lst []string) {
    for _, x := range lst {
        fmt.Print(x + " ")
    }
    fmt.Println()
}

func MyPrintIntList(lst []int) {
    tmp := []string{}
    for _, x := range lst {
        tmp = append(tmp, MyIntToString(x))
    }
    MyPrintStringList(tmp)
}

func MyPrintDict(dict map[int]int) {
    tmp := []string{}
    for k, v := range dict {
        tmp = append(tmp, MyIntToString(k) + "->" + MyIntToString(v))
    }
    MyPrintStringList(tmp)
}

func main() {
    MyPrintString("Hello, World!")
    MyPrintString(MyIntToString(MyStringToInt("123")))
    MyPrintString(MyDoubleToString(MyStringToDouble("123.456")))
    MyPrintString(MyBoolToString(false))
    MyPrintString(MyIntToString(MyNullableToInt(MyIntToNullable(18))))
    MyPrintString(MyIntToString(MyNullableToInt(MyIntToNullable(-15))))
    MyPrintString(MyIntToString(MyNullableToInt(MyIntToNullable(0))))
    MyPrintStringList(MyListSorted([]string{"e", "dddd", "ccccc", "bb", "aaa"}))
    MyPrintStringList(MyListSortedByLength([]string{"e", "dddd", "ccccc", "bb", "aaa"}))
    MyPrintString(MyIntToString(MyListReduce(MyListMap(MyListFilter([]int{3, 12, 5, 8, 9, 15, 7, 17, 21, 11})))))
    MyPrintString(MyIntToString(MyListOperations([]int{3, 12, 5, 8, 9, 15, 7, 17, 21, 11})))
    MyPrintDict(MyListToDict([]int{3, 1, 4, 2, 5, 9, 8, 6, 7, 0}))
    MyPrintIntList(MyDictToList(map[int]int{3: 9, 1: 1, 4: 16, 2: 4, 5: 25, 9: 81, 8: 64, 6: 36, 7: 49, 0: 0}))
}