using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

class PolyEvalType {
    public string typeStr;
    public string typeName;
    public PolyEvalType? valueType;
    public PolyEvalType? keyType;

    public PolyEvalType(string typeStr, string typeName, PolyEvalType? valueType, PolyEvalType? keyType) {
        this.typeStr = typeStr;
        this.typeName = typeName;
        this.valueType = valueType;
        this.keyType = keyType;
    }
}

class Example {
    public static PolyEvalType SToType__(string typeStr) {
        if (!typeStr.Contains("<")) {
            return new PolyEvalType(typeStr, typeStr, null, null);
        } else {
            int idx = typeStr.IndexOf("<");
            string typeName = typeStr.Substring(0, idx);
            string otherStr = typeStr.Substring(idx + 1, typeStr.Length - idx - 2);
            if (!otherStr.Contains(",")) {
                PolyEvalType valueType = SToType__(otherStr);
                return new PolyEvalType(typeStr, typeName, valueType, null);
            } else {
                idx = otherStr.IndexOf(",");
                PolyEvalType keyType = SToType__(otherStr.Substring(0, idx));
                PolyEvalType valueType = SToType__(otherStr.Substring(idx + 1));
                return new PolyEvalType(typeStr, typeName, valueType, keyType);
            }
        }
    }

    public static string EscapeString__(string s) {
        StringBuilder newS = new StringBuilder();
        foreach (char c in s) {
            if (c == '\\') {
                newS.Append("\\\\");
            } else if (c == '\"') {
                newS.Append("\\\"");
            } else if (c == '\n') {
                newS.Append("\\n");
            } else if (c == '\t') {
                newS.Append("\\t");
            } else if (c == '\r') {
                newS.Append("\\r");
            } else {
                newS.Append(c);
            }
        }
        return newS.ToString();
    }

    public static string ByBool__(bool value) {
        return value ? "true" : "false";
    }

    public static string ByInt__(int value) {
        return value.ToString();
    }

    public static string ByDouble__(double value) {
        string vs = value.ToString("F6");
        while (vs.EndsWith("0")) {
            vs = vs.Substring(0, vs.Length - 1);
        }
        if (vs.EndsWith(".")) {
            vs += "0";
        }
        if (vs == "-0.0") {
            vs = "0.0";
        }
        return vs;
    }

    public static string ByString__(string value) {
        return "\"" + EscapeString__(value) + "\"";
    }

    public static string ByList__(IList<object> value, PolyEvalType ty) {
        var vStrs = value.Select(v => ValToS__(v, ty.valueType));
        return "[" + string.Join(", ", vStrs) + "]";
    }

    public static string ByUlist__(IList<object> value, PolyEvalType ty) {
        var vStrs = value.Select(v => ValToS__(v, ty.valueType));
        return "[" + string.Join(", ", vStrs.OrderBy(x => x)) + "]";
    }

    public static string ByDict__(IDictionary<object, object> value, PolyEvalType ty) {
        var vStrs = value.Select(kv => ValToS__(kv.Key, ty.keyType) + "=>" + ValToS__(kv.Value, ty.valueType));
        return "{" + string.Join(", ", vStrs.OrderBy(x => x)) + "}";
    }

    public static string ByOption__(object value, PolyEvalType ty) {
        if (value == null) {
            return "null";
        } else {
            return ValToS__(value, ty.valueType);
        }
    }

    public static string ValToS__(object value, PolyEvalType ty) {
        string typeName = ty.typeName;
        if (typeName == "bool") {
            if (!(value is bool)) {
                throw new Exception("Type mismatch");
            }
            return ByBool__((bool)value);
        } else if (typeName == "int") {
            if (!(value is int) && !(value is double d && d == Math.Floor(d))) {
                throw new Exception("Type mismatch");
            }
            return ByInt__((int)value);
        } else if (typeName == "double") {
            if (!(value is int) && !(value is double)) {
                throw new Exception("Type mismatch");
            }
            return ByDouble__((double)value);
        } else if (typeName == "str") {
            if (!(value is string)) {
                throw new Exception("Type mismatch");
            }
            return ByString__((string)value);
        } else if (typeName == "list") {
            if (!(value is IList)) {
                throw new Exception("Type mismatch");
            }
            List<object> listValue = new List<object>();
            foreach (object v in (IList) value) {
                listValue.Add(v);
            }
            return ByList__(listValue, ty);
        } else if (typeName == "ulist") {
            if (!(value is IList)) {
                throw new Exception("Type mismatch");
            }
            List<object> listValue = new List<object>();
            foreach (object v in (IList) value) {
                listValue.Add(v);
            }
            return ByUlist__(listValue, ty);
        } else if (typeName == "dict") {
            if(!(value is IDictionary)) {
                throw new Exception("Type mismatch");
            }
            Dictionary<object, object> dictValue = new Dictionary<object, object>();
            foreach (DictionaryEntry kv in (IDictionary) value) {
                dictValue[kv.Key] = kv.Value;
            }
            return ByDict__(dictValue, ty);
        } else if (typeName == "option") {
            return ByOption__(value, ty);
        }
        throw new Exception($"Unknown type {typeName}");
    }

    public static string Stringify__(object value, string typeStr) {
        return ValToS__(value, SToType__(typeStr)) + ":" + typeStr;
    }
    
    public static void Main() {
        string tfs = Stringify__(true, "bool") + "\n"
            + Stringify__(3, "int") + "\n"
            + Stringify__(3.141592653, "double") + "\n"
            + Stringify__(3.0, "double") + "\n"
            + Stringify__("Hello, World!", "str") + "\n"
            + Stringify__("!@#$%^&*()\\\"\n\t", "str") + "\n"
            + Stringify__(new List<int> {1, 2, 3}, "list<int>") + "\n"
            + Stringify__(new List<bool> {true, false, true}, "list<bool>") + "\n"
            + Stringify__(new List<int> {3, 2, 1}, "ulist<int>") + "\n"
            + Stringify__(new Dictionary<int, string> {{1, "one"}, {2, "two"}}, "dict<int,str>") + "\n"
            + Stringify__(new Dictionary<string, List<int>> {{"one", new List<int> {1, 2, 3}}, {"two", new List<int> {4, 5, 6}}}, "dict<str,list<int>>") + "\n"
            + Stringify__(null, "option<int>") + "\n"
            + Stringify__(3, "option<int>") + "\n";
        System.IO.File.WriteAllText("stringify.out", tfs);
    }
}