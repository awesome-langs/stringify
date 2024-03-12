class PolyEvalType {
    String typeStr
    String typeName
    PolyEvalType valueType
    PolyEvalType keyType

    PolyEvalType(String typeStr, String typeName, PolyEvalType valueType, PolyEvalType keyType) {
        this.typeStr = typeStr
        this.typeName = typeName
        this.valueType = valueType
        this.keyType = keyType
    }
}

PolyEvalType sToType__(String typeStr) {
    if (!typeStr.contains("<")) {
        return new PolyEvalType(typeStr, typeStr, null, null)
    } else {
        int idx = typeStr.indexOf("<")
        String typeName = typeStr.substring(0, idx)
        String otherStr = typeStr.substring(idx + 1, typeStr.length() - 1)
        if (!otherStr.contains(",")) {
            PolyEvalType valueType = sToType__(otherStr)
            return new PolyEvalType(typeStr, typeName, valueType, null)
        } else {
            idx = otherStr.indexOf(",")
            PolyEvalType keyType = sToType__(otherStr.substring(0, idx))
            PolyEvalType valueType = sToType__(otherStr.substring(idx + 1))
            return new PolyEvalType(typeStr, typeName, valueType, keyType)
        }
    }
}

String escapeString__(String s) {
    StringBuilder newS = new StringBuilder()
    for (int i = 0; i < s.length(); i++) {
        char c = s.charAt(i)
        if (c == '\\') {
            newS.append("\\\\")
        } else if (c == '\"') {
            newS.append("\\\"")
        } else if (c == '\n') {
            newS.append("\\n")
        } else if (c == '\t') {
            newS.append("\\t")
        } else if (c == '\r') {
            newS.append("\\r")
        } else {
            newS.append(c)
        }
    }
    return newS.toString()
}

String byBool__(boolean value) {
    return value ? "true" : "false"
}

String byInt__(int value) {
    return Integer.toString(value)
}

String byDouble__(double value) {
    String vs = String.format("%.6f", value)
    while (vs.endsWith("0")) {
        vs = vs.substring(0, vs.length() - 1)
    }
    if (vs.endsWith(".")) {
        vs += "0"
    }
    if (vs.equals("-0.0")) {
        vs = "0.0"
    }
    return vs
}

String byString__(String value) {
    return "\"" + escapeString__(value) + "\""
}

String byList__(List value, PolyEvalType ty) {
    def vStrs = value.collect { v -> valToS__(v, ty.valueType) } 
    return "[" + vStrs.join(", ") + "]"
}

String byUlist__(List value, PolyEvalType ty) {
    def vStrs = value.collect { v -> valToS__(v, ty.valueType) }
    return "[" + vStrs.join(", ") + "]"
}

String byDict__(Map value, PolyEvalType ty) {
    def vStrs = value.collect { key, val -> valToS__(key, ty.keyType) + "=>" + valToS__(val, ty.valueType) }
    return "{" + vStrs.join(", ") + "}"
}

String byOption__(Object value, PolyEvalType ty) {
    if (value == null) {
        return "null"
    } else {
        return valToS__(value, ty.valueType)
    }
}

String valToS__(Object value, PolyEvalType ty) {
    String typeName = ty.typeName
    if (typeName.equals("bool")) {
        if (!(value instanceof Boolean)) {
            throw new IllegalArgumentException("Type mismatch")
        }
        return byBool__(value)
    } else if (typeName.equals("int")) {
        if (!(value instanceof Integer)) {
            throw new IllegalArgumentException("Type mismatch")
        }
        return byInt__(value)
    } else if (typeName.equals("double")) {
        if (!(value instanceof Double)) {
            throw new IllegalArgumentException("Type mismatch")
        }
        return byDouble__(value)
    } else if (typeName.equals("str")) {
        if (!(value instanceof String)) {
            throw new IllegalArgumentException("Type mismatch")
        }
        return byString__(value)
    } else if (typeName.equals("list")) {
        if (!(value instanceof List)) {
            throw new IllegalArgumentException("Type mismatch")
        }
        return byList__(value, ty)
    } else if (typeName.equals("ulist")) {
        if (!(value instanceof List)) {
            throw new IllegalArgumentException("Type mismatch")
        }
        return byUlist__(value, ty)
    } else if (typeName.equals("dict")) {
        if (!(value instanceof Map)) {
            throw new IllegalArgumentException("Type mismatch")
        }
        return byDict__(value, ty)
    } else if (typeName.equals("option")) {
        return byOption__(value, ty)
    }
    throw new IllegalArgumentException("Unknown type " + typeName)
}

String stringify__(Object value, String typeStr) {
    return valToS__(value, sToType__(typeStr)) + ":" + typeStr
}

def tfs = stringify__(true, "bool") + "\n" +
    stringify__(3, "int") + "\n" +
    stringify__(3.141592653d, "double") + "\n" +
    stringify__(3.0d, "double") + "\n" +
    stringify__("Hello, World!", "str") + "\n" +
    stringify__("!@#\$%^&*()\\\"\n\t", "str") + "\n" +
    stringify__([1, 2, 3], "list<int>") + "\n" +
    stringify__([true, false, true], "list<bool>") + "\n" +
    stringify__([3, 2, 1], "ulist<int>") + "\n" +
    stringify__([1: "one", 2: "two"], "dict<int,str>") + "\n" +
    stringify__(["one": [1, 2, 3], "two": [4, 5, 6]], "dict<str,list<int>>") + "\n" +
    stringify__(null, "option<int>") + "\n" +
    stringify__(3, "option<int>") + "\n"
new File("stringify.out").text = tfs

