import strutils
import options
import algorithm
import sequtils
import tables
import typetraits

type
    PolyEvalType = ref object
        typeStr: string
        typeName: string
        valueType: PolyEvalType
        keyType: PolyEvalType

proc newPolyEvalType(typeStr: string, typeName: string, valueType: PolyEvalType, keyType: PolyEvalType): PolyEvalType =
    new result
    result.typeStr = typeStr
    result.typeName = typeName
    result.valueType = valueType
    result.keyType = keyType
    

proc valToS[T](value: T, ty: PolyEvalType): string

proc sToType(typeStr: string): PolyEvalType =
    if typeStr.contains("<"):
        let idx = typeStr.find("<")
        let typeName = typeStr[0 ..< idx ]
        let otherStr = typeStr[idx + 1 ..< typeStr.len - 1]
        if otherStr.contains(","):
            let idx = otherStr.find(",")
            let keyType = sToType(otherStr[0 ..< idx ])
            let valueType = sToType(otherStr[idx + 1 ..< otherStr.len])
            newPolyEvalType(typeStr, typeName, valueType, keyType)
        else:
            let valueType = sToType(otherStr)
            newPolyEvalType(typeStr, typeName, valueType, nil)
    else:
        newPolyEvalType(typeStr, typeStr, nil, nil)

proc escapeString(s: string): string =
    var newS = ""
    for c in s:
        case c
        of '\\': newS.add("\\\\")
        of '"': newS.add("\\\"")
        of '\n': newS.add("\\n")
        of '\t': newS.add("\\t")
        of '\r': newS.add("\\r")
        else: newS.add(c)
    newS

proc byBool(value: bool): string =
    if value: "true" else: "false"

proc byInt(value: int): string =
    $value

proc byDouble(value: float): string =
    var vs = value.formatFloat(ffDecimal, 6)
    while vs.endsWith("0"):
        vs = vs[0 ..< vs.len - 1]
    if vs.endsWith("."):
        vs.add('0')
    if vs == "-0.0":
        vs = "0.0"
    vs

proc byString(value: string): string =
    '"' & escapeString(value) & '"'

proc byList(value: seq[auto], ty: PolyEvalType): string =
    var vStrs = value.mapIt(valToS(it, ty.valueType))
    "[" & vStrs.join(", ") & "]"

proc byUlist(value: seq[auto], ty: PolyEvalType): string =
    var vStrs = value.mapIt(valToS(it, ty.valueType))
    "[" & vStrs.sorted.join(", ") & "]"

proc byDict[K, V](value: Table[K, V], ty: PolyEvalType): string =
    var vStrs = toSeq(value.pairs).mapIt(valToS(it[0], ty.keyType) & "=>" & valToS(it[1], ty.valueType))
    "{" & vStrs.sorted.join(", ") & "}"

proc byOption(value: Option[auto], ty: PolyEvalType): string =
    if value.isSome:
        valToS(value.get, ty.valueType)
    else:
        "null"

proc valToS[T](value: T, ty: PolyEvalType): string =
    let typeName = ty.typeName
    case typeName
    of "bool": 
        when T is bool: 
            byBool(value) 
        else: 
            raise newException(ValueError, "Type mismatch")
    of "int": 
        when T is int: 
            byInt(value)
        else: 
            raise newException(ValueError, "Type mismatch")
    of "double": 
        when T is float: 
            byDouble(value) 
        else: 
            raise newException(ValueError, "Type mismatch")
    of "str": 
        when T is string: 
            byString(value) 
        else: 
            raise newException(ValueError, "Type mismatch")
    of "list": 
        when T is seq: 
            byList(value, ty) 
        else: 
            raise newException(ValueError, "Type mismatch")
    of "ulist": 
        when T is seq: 
            byUlist(value, ty) 
        else: 
            raise newException(ValueError, "Type mismatch")
    of "dict": 
        when T is Table: 
            byDict(value, ty) 
        else:
            raise newException(ValueError, "Type mismatch")
    of "option":
        when T is Option: 
            byOption(value, ty)
        else:
            raise newException(ValueError, "Type mismatch")
    else: raise newException(ValueError, "Unknown type " & typeName)

proc stringify[T](value: T, typeStr: string): string =
    valToS(value, sToType(typeStr)) & ":" & typeStr

var tfs = stringify(true, "bool") & "\n" &
    stringify(3, "int") & "\n" &
    stringify(3.141592653, "double") & "\n" &
    stringify(3.0, "double") & "\n" &
    stringify("Hello, World!", "str") & "\n" &
    stringify("!@#$%^&*()\\\"\n\t", "str") & "\n" &
    stringify(@[1, 2, 3], "list<int>") & "\n" &
    stringify(@[true, false, true], "list<bool>") & "\n" &
    stringify(@[3, 2, 1], "ulist<int>") & "\n" &
    stringify({1: "one", 2: "two"}.toTable, "dict<int,str>") & "\n" &
    stringify({"one": @[1, 2, 3], "two": @[4, 5, 6]}.toTable, "dict<str,list<int>>") & "\n" &
    stringify(none(int), "option<int>") & "\n" &
    stringify(some(3), "option<int>") & "\n"
writeFile("stringify.out", tfs)