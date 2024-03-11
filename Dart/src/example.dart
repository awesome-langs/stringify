import "dart:io";

class PolyEvalType {
    String typeStr = "";
    String typeName = "";
    PolyEvalType? valueType;
    PolyEvalType? keyType;

    PolyEvalType(String typeStr, String typeName, PolyEvalType? valueType, PolyEvalType? keyType) {
        this.typeStr = typeStr;
        this.typeName = typeName;
        this.valueType = valueType;
        this.keyType = keyType;
    }
}

PolyEvalType __sToType(String typeStr) {
    if (!typeStr.contains("<")) {
        return PolyEvalType(typeStr, typeStr, null, null);
    } else {
        int idx = typeStr.indexOf("<");
        String typeName = typeStr.substring(0, idx);
        String otherStr = typeStr.substring(idx + 1, typeStr.length - 1);
        if (!otherStr.contains(",")) {
            PolyEvalType valueType = __sToType(otherStr);
            return PolyEvalType(typeStr, typeName, valueType, null);
        } else {
            idx = otherStr.indexOf(",");
            PolyEvalType keyType = __sToType(otherStr.substring(0, idx));
            PolyEvalType valueType = __sToType(otherStr.substring(idx + 1));
            return PolyEvalType(typeStr, typeName, valueType, keyType);
        }
    }
}

String __escapeString(String s) {
    List<String> newS = [];
    for (var c in s.split("")) {
        if (c == "\\") {
            newS.add("\\\\");
        } else if (c == "\"") {
            newS.add("\\\"");
        } else if (c == "\n") {
            newS.add("\\n");
        } else if (c == "\t") {
            newS.add("\\t");
        } else if (c == "\r") {
            newS.add("\\r");
        } else {
            newS.add(c);
        }
    }
    return newS.join();
}

String __byBool(bool value) {
    return value ? "true" : "false";
}

String __byInt(int value) {
    return value.toString();
}

String __byDouble(double value) {
    String vs = value.toStringAsFixed(6);
    while (vs.endsWith("0")) {
        vs = vs.substring(0, vs.length - 1);
    }
    if (vs.endsWith(".")) {
        vs += "0";
    }
    if (vs == "-0.0") {
        vs = "0.0";
    }
    return vs;
}

String __byString(String value) {
    return '"' + __escapeString(value) + '"';
}

String __byList(List value, PolyEvalType ty) {
    var vStrs = value.map((v) => __valToS(v, ty.valueType!)).toList();
    return "[" + vStrs.join(", ") + "]";
}

String __byUlist(List value, PolyEvalType ty) {
    var vStrs = value.map((v) => __valToS(v, ty.valueType!)).toList();
    return "[" + (vStrs..sort()).join(", ") + "]";
}

String __byDict(Map value, PolyEvalType ty) {
    var vStrs = value.entries.map((e) => __valToS(e.key, ty.keyType!) + "=>" + __valToS(e.value, ty.valueType!)).toList();
    return "{" + (vStrs..sort()).join(", ") + "}";
}

String __byOption(value, PolyEvalType ty) {
    if (value == null) {
        return "null";
    } else {
        return __valToS(value, ty.valueType!);
    }
}

String __valToS(value, PolyEvalType ty) {
    String typeName = ty.typeName;
    if (typeName == "bool") {
        if (value is! bool) {
            throw ArgumentError("Type mismatch");
        }
        return __byBool(value);
    } else if (typeName == "int") {
        if (value is! int) {
            throw ArgumentError("Type mismatch");
        }
        return __byInt(value);
    } else if (typeName == "double") {
        if (value is! double) {
            throw ArgumentError("Type mismatch");
        }
        return __byDouble(value);
    } else if (typeName == "str") {
        if (value is! String) {
            throw ArgumentError("Type mismatch");
        }
        return __byString(value);
    } else if (typeName == "list") {
        if (value is! List) {
            throw ArgumentError("Type mismatch");
        }
        return __byList(value, ty);
    } else if (typeName == "ulist") {
        if (value is! List) {
            throw ArgumentError("Type mismatch");
        }
        return __byUlist(value, ty);
    } else if (typeName == "dict") {
        if (value is! Map) {
            throw ArgumentError("Type mismatch");
        }
        return __byDict(value, ty);
    } else if (typeName == "option") {
        return __byOption(value, ty);
    }
    throw ArgumentError("Unknown type $typeName");
}

String __stringify(value, String typeStr) {
    return __valToS(value, __sToType(typeStr)) + ":" + typeStr;
}

void main() {
    String tfs = __stringify(true, "bool") + "\n" +
        __stringify(3, "int") + "\n" +
        __stringify(3.141592653, "double") + "\n" +
        __stringify(3.0, "double") + "\n" +
        __stringify("Hello, World!", "str") + "\n" +
        __stringify("!@#\$%^&*()\\\"\n\t", "str") + "\n" +
        __stringify([1, 2, 3], "list<int>") + "\n" +
        __stringify([true, false, true], "list<bool>") + "\n" +
        __stringify([3, 2, 1], "ulist<int>") + "\n" +
        __stringify({1: "one", 2: "two"}, "dict<int,str>") + "\n" +
        __stringify({"one": [1, 2, 3], "two": [4, 5, 6]}, "dict<str,list<int>>") + "\n" +
        __stringify(null, "option<int>") + "\n" +
        __stringify(3, "option<int>") + "\n";
    File f = File('stringify.out');
    f.writeAsStringSync(tfs);
}