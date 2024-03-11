class PolyEvalType
    constructor: (typeStr, typeName, valueType, keyType) ->
        @typeStr = typeStr
        @typeName = typeName
        @valueType = valueType
        @keyType = keyType

sToType__ = (typeStr) ->
    if !typeStr.includes "<"
        return new PolyEvalType typeStr, typeStr, null, null
    else
        idx = typeStr.indexOf "<"
        typeName = typeStr.substring 0, idx
        otherStr = typeStr.substring idx + 1, typeStr.length - 1
        if !otherStr.includes ","
            valueType = sToType__ otherStr
            return new PolyEvalType typeStr, typeName, valueType, null
        else
            idx = otherStr.indexOf ","
            keyType = sToType__ otherStr.substring 0, idx
            valueType = sToType__ otherStr.substring idx + 1
            return new PolyEvalType typeStr, typeName, valueType, keyType

escapeString__ = (s) ->
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

byBool__ = (value) ->
    return if value then "true" else "false"

byInt__ = (value) ->
    v = parseInt value
    return v.toString()

byDouble__ = (value) ->
    v = parseFloat value
    vs = v.toFixed 6
    while vs.endsWith "0"
        vs = vs.substring 0, vs.length - 1
    if vs.endsWith "."
        vs += "0"
    if vs is "-0.0"
        vs = "0.0"
    return vs

byString__ = (value) ->
    return '"' + escapeString__(value) + '"'

byList__ = (value, ty) ->
    vStrs = value.map (v) -> valToS__ v, ty.valueType
    return "[" + vStrs.join(", ") + "]"

byUlist__ = (value, ty) ->
    vStrs = value.map (v) -> valToS__ v, ty.valueType
    return "[" + vStrs.sort().join(", ") + "]"

byDict__ = (value, ty) ->
    vStrs = [value...].map ([key, val]) -> valToS__(key, ty.keyType) + "=>" + valToS__(val, ty.valueType)
    return "{" + vStrs.sort().join(", ") + "}"

byOption__ = (value, ty) ->
    if value is null
        return "null"
    else
        return valToS__ value, ty.valueType

valToS__ = (value, ty) ->
    typeName = ty.typeName
    if typeName is "bool"
        if typeof value isnt "boolean"
            throw new Error "Type mismatch"
        return byBool__ value
    else if typeName is "int"
        if typeof value isnt "number" and !Number.isInteger value
            throw new Error "Type mismatch"
        return byInt__ value
    else if typeName is "double"
        if typeof value isnt "number"
            throw new Error "Type mismatch"
        return byDouble__ value
    else if typeName is "str"
        if typeof value isnt "string"
            throw new Error "Type mismatch"
        return byString__ value
    else if typeName is "list"
        if !Array.isArray value
            throw new Error "Type mismatch"
        return byList__ value, ty
    else if typeName is "ulist"
        if !Array.isArray value
            throw new Error "Type mismatch"
        return byUlist__ value, ty
    else if typeName is "dict"
        if !(value instanceof Map)
            throw new Error "Type mismatch"
        return byDict__ value, ty
    else if typeName is "option"
        return byOption__ value, ty
    throw new Error "Unknown type #{typeName}"

stringify__ = (value, typeStr) ->
    return valToS__(value, sToType__(typeStr)) + ":" + typeStr

tfs = stringify__(true, "bool") + "\n" \
    + stringify__(3, "int") + "\n" \
    + stringify__(3.141592653, "double") + "\n" \
    + stringify__(3.0, "double") + "\n" \
    + stringify__("Hello, World!", "str") + "\n" \
    + stringify__("!@#$%^&*()\\\"\n\t", "str") + "\n" \
    + stringify__([1, 2, 3], "list<int>") + "\n" \
    + stringify__([true, false, true], "list<bool>") + "\n" \
    + stringify__([3, 2, 1], "ulist<int>") + "\n" \
    + stringify__(new Map([[1, "one"], [2, "two"]]), "dict<int,str>") + "\n" \
    + stringify__(new Map([["one", [1, 2, 3]], ["two", [4, 5, 6]]]), "dict<str,list<int>>") + "\n" \
    + stringify__(null, "option<int>") + "\n" \
    + stringify__(3, "option<int>") + "\n"
fs = require "fs"
fs.writeFileSync "stringify.out", tfs
