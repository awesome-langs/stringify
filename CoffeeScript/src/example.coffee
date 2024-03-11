class PolyEvalType
    constructor: (typeStr, typeName, valueType, keyType) ->
        @typeStr = typeStr
        @typeName = typeName
        @valueType = valueType
        @keyType = keyType

__sToType = (typeStr) ->
    if !typeStr.includes "<"
        return new PolyEvalType typeStr, typeStr, null, null
    else
        idx = typeStr.indexOf "<"
        typeName = typeStr.substring 0, idx
        otherStr = typeStr.substring idx + 1, typeStr.length - 1
        if !otherStr.includes ","
            valueType = __sToType otherStr
            return new PolyEvalType typeStr, typeName, valueType, null
        else
            idx = otherStr.indexOf ","
            keyType = __sToType otherStr.substring 0, idx
            valueType = __sToType otherStr.substring idx + 1
            return new PolyEvalType typeStr, typeName, valueType, keyType

__escapeString = (s) ->
    newS = []
    for c in s
        switch c
            when "\\" then newS.push "\\"
            when "\"" then newS.push "\\\""
            when "\n" then newS.push "\\n"
            when "\t" then newS.push "\\t"
            when "\r" then newS.push "\\r"
            else newS.push c
    return newS.join ""

__byBool = (value) ->
    return if value then "true" else "false"

__byInt = (value) ->
    v = parseInt value
    return v.toString()

__byDouble = (value) ->
    v = parseFloat value
    vs = v.toFixed 6
    while vs.endsWith "0"
        vs = vs.substring 0, vs.length - 1
    if vs.endsWith "."
        vs += "0"
    if vs is "-0.0"
        vs = "0.0"
    return vs

__byString = (value) ->
    return '"' + __escapeString(value) + '"'

__byList = (value, ty) ->
    vStrs = value.map (v) -> __valToS v, ty.valueType
    return "[" + vStrs.join(", ") + "]"

__byUlist = (value, ty) ->
    vStrs = value.map (v) -> __valToS v, ty.valueType
    return "[" + vStrs.sort().join(", ") + "]"

__byDict = (value, ty) ->
    vStrs = [value...].map ([key, val]) -> __valToS(key, ty.keyType) + "=>" + __valToS(val, ty.valueType)
    return "{" + vStrs.sort().join(", ") + "}"

__byOption = (value, ty) ->
    if value is null
        return "null"
    else
        return __valToS value, ty.valueType

__valToS = (value, ty) ->
    typeName = ty.typeName
    if typeName is "bool"
        if typeof value isnt "boolean"
            throw new Error "Type mismatch"
        return __byBool value
    else if typeName is "int"
        if typeof value isnt "number" and !Number.isInteger value
            throw new Error "Type mismatch"
        return __byInt value
    else if typeName is "double"
        if typeof value isnt "number"
            throw new Error "Type mismatch"
        return __byDouble value
    else if typeName is "str"
        if typeof value isnt "string"
            throw new Error "Type mismatch"
        return __byString value
    else if typeName is "list"
        if !Array.isArray value
            throw new Error "Type mismatch"
        return __byList value, ty
    else if typeName is "ulist"
        if !Array.isArray value
            throw new Error "Type mismatch"
        return __byUlist value, ty
    else if typeName is "dict"
        if !(value instanceof Map)
            throw new Error "Type mismatch"
        return __byDict value, ty
    else if typeName is "option"
        return __byOption value, ty
    throw new Error "Unknown type #{typeName}"

__stringify = (value, typeStr) ->
    return __valToS(value, __sToType(typeStr)) + ":" + typeStr

tfs = __stringify(true, "bool") + "\n" \
    + __stringify(3, "int") + "\n" \
    + __stringify(3.141592653, "double") + "\n" \
    + __stringify(3.0, "double") + "\n" \
    + __stringify("Hello, World!", "str") + "\n" \
    + __stringify("!@#$%^&*()\\\"\n\t", "str") + "\n" \
    + __stringify([1, 2, 3], "list<int>") + "\n" \
    + __stringify([true, false, true], "list<bool>") + "\n" \
    + __stringify([3, 2, 1], "ulist<int>") + "\n" \
    + __stringify(new Map([[1, "one"], [2, "two"]]), "dict<int,str>") + "\n" \
    + __stringify(new Map([["one", [1, 2, 3]], ["two", [4, 5, 6]]]), "dict<str,list<int>>") + "\n" \
    + __stringify(null, "option<int>") + "\n" \
    + __stringify(3, "option<int>") + "\n"
fs = require "fs"
fs.writeFileSync "stringify.out", tfs
