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

PolyEvalType sToType__(String typeStr) {
    if (!typeStr.contains("<")) {
        return PolyEvalType(typeStr, typeStr, null, null);
    } else {
        int idx = typeStr.indexOf("<");
        String typeName = typeStr.substring(0, idx);
        String otherStr = typeStr.substring(idx + 1, typeStr.length - 1);
        if (!otherStr.contains(",")) {
            PolyEvalType valueType = sToType__(otherStr);
            return PolyEvalType(typeStr, typeName, valueType, null);
        } else {
            idx = otherStr.indexOf(",");
            PolyEvalType keyType = sToType__(otherStr.substring(0, idx));
            PolyEvalType valueType = sToType__(otherStr.substring(idx + 1));
            return PolyEvalType(typeStr, typeName, valueType, keyType);
        }
    }
}

String escapeString__(String s) {
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

String byBool__(bool value) {
    return value ? "true" : "false";
}

String byInt__(int value) {
    return value.toString();
}

String byDouble__(double value) {
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

String byString__(String value) {
    return '"' + escapeString__(value) + '"';
}

String byList__(List value, PolyEvalType ty) {
    var vStrs = value.map((v) => valToS__(v, ty.valueType!)).toList();
    return "[" + vStrs.join(", ") + "]";
}

String byUlist__(List value, PolyEvalType ty) {
    var vStrs = value.map((v) => valToS__(v, ty.valueType!)).toList();
    return "[" + (vStrs..sort()).join(", ") + "]";
}

String byDict__(Map value, PolyEvalType ty) {
    var vStrs = value.entries.map((e) => valToS__(e.key, ty.keyType!) + "=>" + valToS__(e.value, ty.valueType!)).toList();
    return "{" + (vStrs..sort()).join(", ") + "}";
}

String byOption__(value, PolyEvalType ty) {
    if (value == null) {
        return "null";
    } else {
        return valToS__(value, ty.valueType!);
    }
}

String valToS__(value, PolyEvalType ty) {
    String typeName = ty.typeName;
    if (typeName == "bool") {
        if (value is! bool) {
            throw ArgumentError("Type mismatch");
        }
        return byBool__(value);
    } else if (typeName == "int") {
        if (value is! int) {
            throw ArgumentError("Type mismatch");
        }
        return byInt__(value);
    } else if (typeName == "double") {
        if (value is! double) {
            throw ArgumentError("Type mismatch");
        }
        return byDouble__(value);
    } else if (typeName == "str") {
        if (value is! String) {
            throw ArgumentError("Type mismatch");
        }
        return byString__(value);
    } else if (typeName == "list") {
        if (value is! List) {
            throw ArgumentError("Type mismatch");
        }
        return byList__(value, ty);
    } else if (typeName == "ulist") {
        if (value is! List) {
            throw ArgumentError("Type mismatch");
        }
        return byUlist__(value, ty);
    } else if (typeName == "dict") {
        if (value is! Map) {
            throw ArgumentError("Type mismatch");
        }
        return byDict__(value, ty);
    } else if (typeName == "option") {
        return byOption__(value, ty);
    }
    throw ArgumentError("Unknown type $typeName");
}

String stringify__(value, String typeStr) {
    return valToS__(value, sToType__(typeStr)) + ":" + typeStr;
}

void main() {
    String tfs = stringify__(true, "bool") + "\n" +
        stringify__(3, "int") + "\n" +
        stringify__(3.141592653, "double") + "\n" +
        stringify__(3.0, "double") + "\n" +
        stringify__("Hello, World!", "str") + "\n" +
        stringify__("!@#\$%^&*()\\\"\n\t", "str") + "\n" +
        stringify__([1, 2, 3], "list<int>") + "\n" +
        stringify__([true, false, true], "list<bool>") + "\n" +
        stringify__([3, 2, 1], "ulist<int>") + "\n" +
        stringify__({1: "one", 2: "two"}, "dict<int,str>") + "\n" +
        stringify__({"one": [1, 2, 3], "two": [4, 5, 6]}, "dict<str,list<int>>") + "\n" +
        stringify__(null, "option<int>") + "\n" +
        stringify__(3, "option<int>") + "\n";
    File f = File('stringify.out');
    f.writeAsStringSync(tfs);
}