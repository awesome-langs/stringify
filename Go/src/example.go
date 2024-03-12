package main
import (
    "fmt"
    "os"
    "sort"
    "strings"
    "reflect"
)

type PolyEvalType struct {
    typeStr   string
    typeName  string
    valueType *PolyEvalType
    keyType   *PolyEvalType
}

func NewPolyEvalType__(typeStr string, typeName string, valueType *PolyEvalType, keyType *PolyEvalType) *PolyEvalType {
    return &PolyEvalType{typeStr, typeName, valueType, keyType}
}

func SToType__(typeStr string) *PolyEvalType {
    if !strings.Contains(typeStr, "<") {
        return NewPolyEvalType__(typeStr, typeStr, nil, nil)
    } else {
        idx := strings.Index(typeStr, "<")
        typeName := typeStr[:idx]
        otherStr := typeStr[idx+1 : len(typeStr)-1]
        if !strings.Contains(otherStr, ",") {
            valueType := SToType__(otherStr)
            return NewPolyEvalType__(typeStr, typeName, valueType, nil)
        } else {
            idx = strings.Index(otherStr, ",")
            keyType := SToType__(otherStr[:idx])
            valueType := SToType__(otherStr[idx+1:])
            return NewPolyEvalType__(typeStr, typeName, valueType, keyType)
        }
    }
}

func EscapeString__(s string) string {
    newS := make([]string, 0)
    for _, c := range s {
        if c == '\\' {
            newS = append(newS, "\\\\")
        } else if c == '"' {
            newS = append(newS, "\\\"")
        } else if c == '\n' {
            newS = append(newS, "\\n")
        } else if c == '\t' {
            newS = append(newS, "\\t")
        } else if c == '\r' {
            newS = append(newS, "\\r")
        } else {
            newS = append(newS, string(c))
        }
    }
    return strings.Join(newS, "")
}

func byBool__(value bool) string {
    if value {
        return "true"
    } else {
        return "false"
    }
}

func byInt__(value int) string {
    return fmt.Sprintf("%d", value)
}

func byDouble__(value float64) string {
    vs := fmt.Sprintf("%.6f", value)
    for strings.HasSuffix(vs, "0") {
        vs = vs[:len(vs)-1]
    }
    if strings.HasSuffix(vs, ".") {
        vs += "0"
    }
    if vs == "-0.0" {
        vs = "0.0"
    }
    return vs
}

func byString__(value string) string {
    return "\"" + EscapeString__(value) + "\""
}

func byList__(value []interface{}, ty *PolyEvalType) string {
    vStrs := make([]string, 0)
    for _, v := range value {
        vStrs = append(vStrs, valToS(v, ty.valueType))
    }
    return "[" + strings.Join(vStrs, ", ") + "]"
}

func byUlist__(value []interface{}, ty *PolyEvalType) string {
    vStrs := make([]string, 0)
    for _, v := range value {
        vStrs = append(vStrs, valToS(v, ty.valueType))
    }
    sort.Strings(vStrs)
    return "[" + strings.Join(vStrs, ", ") + "]"
}

func byDict__(value map[interface{}]interface{}, ty *PolyEvalType) string {
    vStrs := make([]string, 0)
    for key, val := range value {
        vStrs = append(vStrs, valToS(key, ty.keyType)+"=>"+valToS(val, ty.valueType))
    }
    sort.Strings(vStrs)
    return "{" + strings.Join(vStrs, ", ") + "}"
}

func byOption__(value interface{}, ty *PolyEvalType) string {
    if value == nil {
        return "null"
    } else if reflect.TypeOf(value).Kind() != reflect.Ptr {
        panic("Type mismatch")
    }
    ptrValue := reflect.ValueOf(value)
    ptr := ptrValue.Elem().Interface()
    return valToS(ptr, ty.valueType)
}

func valToS(value interface{}, ty *PolyEvalType) string {
    typeName := ty.typeName
    if typeName == "bool" {
        if _, ok := value.(bool); !ok {
            panic("Type mismatch")
        }
        return byBool__(value.(bool))
    } else if typeName == "int" {
        if _, ok := value.(int); !ok {
            panic("Type mismatch")
        }
        return byInt__(value.(int))
    } else if typeName == "double" {
        if _, ok := value.(float64); !ok {
            panic("Type mismatch")
        }
        return byDouble__(value.(float64))
    } else if typeName == "str" {
        if _, ok := value.(string); !ok {
            panic("Type mismatch")
        }
        return byString__(value.(string))
    } else if typeName == "list" {
        if reflect.TypeOf(value).Kind() != reflect.Slice {
            panic("Type mismatch")
        }
        sliceValue := reflect.ValueOf(value)
        interfaceSlice := make([]interface{}, sliceValue.Len())
        for i := 0; i < sliceValue.Len(); i++ {
            interfaceSlice[i] = sliceValue.Index(i).Interface()
        }
        return byList__(interfaceSlice, ty)
    } else if typeName == "ulist" {
        if reflect.TypeOf(value).Kind() != reflect.Slice {
            panic("Type mismatch")
        }
        sliceValue := reflect.ValueOf(value)
        interfaceSlice := make([]interface{}, sliceValue.Len())
        for i := 0; i < sliceValue.Len(); i++ {
            interfaceSlice[i] = sliceValue.Index(i).Interface()
        }
        return byUlist__(interfaceSlice, ty)
    } else if typeName == "dict" {
        if reflect.TypeOf(value).Kind() != reflect.Map {
            panic("Type mismatch")
        }
        mapValue := reflect.ValueOf(value)
        interfaceMap := make(map[interface{}]interface{})
        for _, key := range mapValue.MapKeys() {
            interfaceMap[key.Interface()] = mapValue.MapIndex(key).Interface()
        }
        return byDict__(interfaceMap, ty)
    } else if typeName == "option" {
        return byOption__(value, ty)
    } else {
        panic(fmt.Sprintf("Unknown type %s", typeName))
    }
}

func stringify__(value interface{}, typeStr string) string {
    return valToS(value, SToType__(typeStr)) + ":" + typeStr
}

func main() {
    tfs := stringify__(true, "bool") + "\n" +
          stringify__(3, "int") + "\n" +
          stringify__(3.141592653, "double") + "\n" +
          stringify__(3.0, "double") + "\n" +
          stringify__("Hello, World!", "str") + "\n" +
          stringify__("!@#$%^&*()\\\"\n\t", "str") + "\n" +
          stringify__([]int{1, 2, 3}, "list<int>") + "\n" +
          stringify__([]bool{true, false, true}, "list<bool>") + "\n" +
          stringify__([]int{3, 2, 1}, "ulist<int>") + "\n" +
          stringify__(map[int]string{1: "one", 2: "two"}, "dict<int,str>") + "\n" +
          stringify__(map[string][]int{"one": []int{1, 2, 3}, "two": []int{4, 5, 6}}, "dict<str,list<int>>") + "\n" +
          stringify__(nil, "option<int>") + "\n" +
          stringify__(&[]int{3}[0], "option<int>") + "\n"
    f, _ := os.Create("stringify.out")
    f.WriteString(tfs)
    f.Close()
}