import std.stdio;
import std.string;
import std.conv;
import std.algorithm;
import std.typecons;

class PolyEvalType {
    public string typeStr;
    public string typeName;
    public PolyEvalType valueType;
    public PolyEvalType keyType;

    this(string typeStr, string typeName, PolyEvalType valueType, PolyEvalType keyType) {
        this.typeStr = typeStr;
        this.typeName = typeName;
        this.valueType = valueType;
        this.keyType = keyType;
    }
}

PolyEvalType __sToType(string typeStr) {
    if (typeStr.indexOf("<") == -1) {
        return new PolyEvalType(typeStr, typeStr, null, null);
    } else {
        size_t idx = typeStr.indexOf("<");
        string typeName = typeStr[0 .. idx];
        string otherStr = typeStr[idx + 1 .. $ - 1];
        if (otherStr.indexOf(",") == -1) {
            PolyEvalType valueType = __sToType(otherStr);
            return new PolyEvalType(typeStr, typeName, valueType, null);
        } else {
            idx = otherStr.indexOf(",");
            PolyEvalType keyType = __sToType(otherStr[0 .. idx]);
            PolyEvalType valueType = __sToType(otherStr[idx + 1 .. $]);
            return new PolyEvalType(typeStr, typeName, valueType, keyType);
        }
    }
}

string __escapeString(string s) {
    string newS;
    foreach (c; s) {
        if (c == '\\') {
            newS ~= "\\\\";
        } else if (c == '\"') {
            newS ~= "\\\"";
        } else if (c == '\n') {
            newS ~= "\\n";
        } else if (c == '\t') {
            newS ~= "\\t";
        } else if (c == '\r') {
            newS ~= "\\r";
        } else {
            newS ~= c.to!string;
        }
    }
    return newS;
}

string __byBool(bool value) {
    return value ? "true" : "false";
}

string __byInt(int value) {
    return value.to!string;
}

string __byDouble(double value) {
    string vs = format("%.6f", value);
    while (vs.endsWith("0")) {
        vs = vs[0 .. $ - 1];
    }
    if (vs.endsWith(".")) {
        vs ~= "0";
    }
    if (vs == "-0.0") {
        vs = "0.0";
    }
    return vs;
}

string __byString(string value) {
    return `"` ~ __escapeString(value) ~ `"`;
}

string __byList(T)(T value, PolyEvalType ty) if(is(T t == U[], U)) {
    string[] vStrs;
    foreach (v; value) {
        vStrs ~= __valToS(v, ty.valueType);
    }
    string ret = "[";
    foreach (i, vStr; vStrs) {
        ret ~= vStr;
        if (i < vStrs.length - 1) {
            ret ~= ", ";
        }
    }
    ret ~= "]";
    return ret;
}

string __byUlist(T)(T value, PolyEvalType ty) if(is(T t == U[], U)) {
    string[] vStrs;
    foreach (v; value) {
        vStrs ~= __valToS(v, ty.valueType);
    }
    vStrs.sort();
    string ret = "[";
    foreach (i, vStr; vStrs) {
        ret ~= vStr;
        if (i < vStrs.length - 1) {
            ret ~= ", ";
        }
    }
    ret ~= "]";
    return ret;
}

string __byDict(T)(T value, PolyEvalType ty) if(is(T t == K[V], K, V)) {
    string[] vStrs;
    foreach (key, val; value) {
        vStrs ~= __valToS(key, ty.keyType) ~ "=>" ~ __valToS(val, ty.valueType);
    }
    vStrs.sort();
    string ret = "{";
    foreach (i, vStr; vStrs) {
        ret ~= vStr;
        if (i < vStrs.length - 1) {
            ret ~= ", ";
        }
    }
    ret ~= "}";
    return ret;
}

string __byOption(T)(T value, PolyEvalType ty) if(is(T t == Nullable!U, U)) {
    if (value.isNull) {
        return "null";
    } else {
        return __valToS(value.get, ty.valueType);
    }
}

string __valToS(T)(T value, PolyEvalType ty) if (is(T == int) || is(T == double) || is(T == bool) || is(T == string) || is(T == immutable(char)) || is(T t == U[], U) || is(T t == K[V], K, V) || is(T t == Nullable!U, U))  {
    string typeName = ty.typeName;
    if (typeName == "bool") {
        static if (is(T == bool)) {
            return __byBool(value);
        }
        throw new Exception("Type mismatch");        
    } else if (typeName == "int") {
        static if (is(T == int)) {
            return __byInt(value);
        }
        throw new Exception("Type mismatch");
    } else if (typeName == "double") {
       static if (is(T == double)) {
            return __byDouble(value);
        }
        throw new Exception("Type mismatch");
    } else if (typeName == "str") {
        static if (is(T == string) && is(T == immutable(char)[])) {
            return __byString(value);
        }
        throw new Exception("Type mismatch");
    } else if (typeName == "list") {
        static if (is(T t == U[], U)) {
            return __byList(value, ty);
        }
        throw new Exception("Type mismatch");        
    } else if (typeName == "ulist") {
        static if (is(T t == U[], U)) {
            return __byUlist(value, ty);
        }
        throw new Exception("Type mismatch");    
    } else if (typeName == "dict") {
        static if (is(T t == K[V], K, V)) {
            return __byDict(value, ty);
        }
        throw new Exception("Type mismatch");        
    } else if (typeName == "option") {
        static if (is(T t == Nullable!U, U)) {
            return __byOption(value, ty);
        }
        throw new Exception("Type mismatch");
    }
    throw new Exception("Unknown type " ~ typeName);
}

string __stringify(T)(T value, string typeStr) if (is(T == int) || is(T == double) || is(T == bool) || is(T == string) || is(T == immutable(char)) || is(T t == U[], U) || is(T t == K[V], K, V) || is(T t == Nullable!U, U)) {
    return __valToS(value, __sToType(typeStr)) ~ ":" ~ typeStr;
}

void main() {
    string tfs = __stringify(true, "bool") ~ "\n"
      ~ __stringify(3, "int") ~ "\n"
      ~ __stringify(3.141592653, "double") ~ "\n"
      ~ __stringify(3.0, "double") ~ "\n"
      ~ __stringify("Hello, World!", "str") ~ "\n"
      ~ __stringify("!@#$%^&*()\\\"\n\t", "str") ~ "\n"
      ~ __stringify([1, 2, 3], "list<int>") ~ "\n"
      ~ __stringify([true, false, true], "list<bool>") ~ "\n"
      ~ __stringify([3, 2, 1], "ulist<int>") ~ "\n"
      ~ __stringify([1: "one", 2: "two"], "dict<int,str>") ~ "\n"
      ~ __stringify(["one": [1, 2, 3], "two": [4, 5, 6]], "dict<str,list<int>>") ~ "\n"
      ~ __stringify(Nullable!int(), "option<int>") ~ "\n"
      ~ __stringify(Nullable!int(3), "option<int>") ~ "\n";
    File("stringify.out", "w").write(tfs);
}