import std.stdio;
import std.string;
import std.conv;
import std.algorithm;
import std.typecons;
import std.array;

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

PolyEvalType sToType__(string typeStr) {
    if (typeStr.indexOf("<") == -1) {
        return new PolyEvalType(typeStr, typeStr, null, null);
    } else {
        size_t idx = typeStr.indexOf("<");
        string typeName = typeStr[0 .. idx];
        string otherStr = typeStr[idx + 1 .. $ - 1];
        if (otherStr.indexOf(",") == -1) {
            PolyEvalType valueType = sToType__(otherStr);
            return new PolyEvalType(typeStr, typeName, valueType, null);
        } else {
            idx = otherStr.indexOf(",");
            PolyEvalType keyType = sToType__(otherStr[0 .. idx]);
            PolyEvalType valueType = sToType__(otherStr[idx + 1 .. $]);
            return new PolyEvalType(typeStr, typeName, valueType, keyType);
        }
    }
}

string escapeString__(string s) {
    string[] newS;
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
    return newS.join("");
}

string byBool__(bool value) {
    return value ? "true" : "false";
}

string byInt__(int value) {
    return value.to!string;
}

string byDouble__(double value) {
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

string byString__(string value) {
    return `"` ~ escapeString__(value) ~ `"`;
}

string byList__(T)(T value, PolyEvalType ty) if(is(T t == U[], U)) {
    string[] vStrs;
    foreach (v; value) {
        vStrs ~= valToS__(v, ty.valueType);
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

string byUlist__(T)(T value, PolyEvalType ty) if(is(T t == U[], U)) {
    string[] vStrs;
    foreach (v; value) {
        vStrs ~= valToS__(v, ty.valueType);
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

string byDict__(T)(T value, PolyEvalType ty) if(is(T t == K[V], K, V)) {
    string[] vStrs;
    foreach (key, val; value) {
        vStrs ~= valToS__(key, ty.keyType) ~ "=>" ~ valToS__(val, ty.valueType);
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

string byOption__(T)(T value, PolyEvalType ty) if(is(T t == Nullable!U, U)) {
    if (value.isNull) {
        return "null";
    } else {
        return valToS__(value.get, ty.valueType);
    }
}

string valToS__(T)(T value, PolyEvalType ty) if (is(T == int) || is(T == double) || is(T == bool) || is(T == string) || is(T == immutable(char)) || is(T t == U[], U) || is(T t == K[V], K, V) || is(T t == Nullable!U, U))  {
    string typeName = ty.typeName;
    if (typeName == "bool") {
        static if (is(T == bool)) {
            return byBool__(value);
        }
        throw new Exception("Type mismatch");        
    } else if (typeName == "int") {
        static if (is(T == int)) {
            return byInt__(value);
        }
        throw new Exception("Type mismatch");
    } else if (typeName == "double") {
       static if (is(T == double)) {
            return byDouble__(value);
        }
        throw new Exception("Type mismatch");
    } else if (typeName == "str") {
        static if (is(T == string) && is(T == immutable(char)[])) {
            return byString__(value);
        }
        throw new Exception("Type mismatch");
    } else if (typeName == "list") {
        static if (is(T t == U[], U)) {
            return byList__(value, ty);
        }
        throw new Exception("Type mismatch");        
    } else if (typeName == "ulist") {
        static if (is(T t == U[], U)) {
            return byUlist__(value, ty);
        }
        throw new Exception("Type mismatch");    
    } else if (typeName == "dict") {
        static if (is(T t == K[V], K, V)) {
            return byDict__(value, ty);
        }
        throw new Exception("Type mismatch");        
    } else if (typeName == "option") {
        static if (is(T t == Nullable!U, U)) {
            return byOption__(value, ty);
        }
        throw new Exception("Type mismatch");
    }
    throw new Exception("Unknown type " ~ typeName);
}

string stringify__(T)(T value, string typeStr) if (is(T == int) || is(T == double) || is(T == bool) || is(T == string) || is(T == immutable(char)) || is(T t == U[], U) || is(T t == K[V], K, V) || is(T t == Nullable!U, U)) {
    return valToS__(value, sToType__(typeStr)) ~ ":" ~ typeStr;
}

void main() {
    string tfs = stringify__(true, "bool") ~ "\n"
      ~ stringify__(3, "int") ~ "\n"
      ~ stringify__(3.141592653, "double") ~ "\n"
      ~ stringify__(3.0, "double") ~ "\n"
      ~ stringify__("Hello, World!", "str") ~ "\n"
      ~ stringify__("!@#$%^&*()\\\"\n\t", "str") ~ "\n"
      ~ stringify__([1, 2, 3], "list<int>") ~ "\n"
      ~ stringify__([true, false, true], "list<bool>") ~ "\n"
      ~ stringify__([3, 2, 1], "ulist<int>") ~ "\n"
      ~ stringify__([1: "one", 2: "two"], "dict<int,str>") ~ "\n"
      ~ stringify__(["one": [1, 2, 3], "two": [4, 5, 6]], "dict<str,list<int>>") ~ "\n"
      ~ stringify__(Nullable!int(), "option<int>") ~ "\n"
      ~ stringify__(Nullable!int(3), "option<int>") ~ "\n";
    File("stringify.out", "w").write(tfs);
}