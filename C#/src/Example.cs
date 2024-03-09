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
    public static PolyEvalType __SToType(string typeStr) {
        if (!typeStr.Contains("<")) {
            return new PolyEvalType(typeStr, typeStr, null, null);
        } else {
            int idx = typeStr.IndexOf("<");
            string typeName = typeStr.Substring(0, idx);
            string otherStr = typeStr.Substring(idx + 1, typeStr.Length - idx - 2);
            if (!otherStr.Contains(",")) {
                PolyEvalType valueType = __SToType(otherStr);
                return new PolyEvalType(typeStr, typeName, valueType, null);
            } else {
                idx = otherStr.IndexOf(",");
                PolyEvalType keyType = __SToType(otherStr.Substring(0, idx));
                PolyEvalType valueType = __SToType(otherStr.Substring(idx + 1));
                return new PolyEvalType(typeStr, typeName, valueType, keyType);
            }
        }
    }

    public static string __EscapeString(string s) {
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

    public static string __ByBool(bool value) {
        return value ? "true" : "false";
    }

    public static string __ByInt(int value) {
        return value.ToString();
    }

    public static string __ByDouble(double value) {
        if (double.IsNaN(value)) {
            return "nan";
        } else if (double.IsInfinity(value)) {
            if (value > 0) {
                return "inf";
            } else {
                return "-inf";
            }
        }
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

    public static string __ByString(string value) {
        return "\"" + __EscapeString(value) + "\"";
    }

    public static string __ByList(IList<object> value, PolyEvalType t) {
        List<string> vStrs = new List<string>();
        foreach (object v in value) {
            vStrs.Add(__ValToS(v, t.valueType));
        }
        StringBuilder ret = new StringBuilder();
        ret.Append("[");
        for (int i = 0; i < vStrs.Count; i++) {
            ret.Append(vStrs[i]);
            if (i < vStrs.Count - 1) {
                ret.Append(", ");
            }
        }
        ret.Append("]");
        return ret.ToString();
    }

    public static string __ByUlist(IList<object> value, PolyEvalType t) {
        List<string> vStrs = new List<string>();
        foreach (object v in value) {
            vStrs.Add(__ValToS(v, t.valueType));
        }
        vStrs.Sort();
        StringBuilder ret = new StringBuilder();
        ret.Append("[");
        for (int i = 0; i < vStrs.Count; i++) {
            ret.Append(vStrs[i]);
            if (i < vStrs.Count - 1) {
                ret.Append(", ");
            }
        }
        ret.Append("]");
        return ret.ToString();
    }

    public static string __ByDict(IDictionary<object, object> value, PolyEvalType t) {
        List<string> vStrs = new List<string>();
        foreach (KeyValuePair<object, object> kv in value) {
            vStrs.Add(__ValToS(kv.Key, t.keyType) + "=>" + __ValToS(kv.Value, t.valueType));
        }
        vStrs.Sort();
        StringBuilder ret = new StringBuilder();
        ret.Append("{");
        for (int i = 0; i < vStrs.Count; i++) {
            ret.Append(vStrs[i]);
            if (i < vStrs.Count - 1) {
                ret.Append(", ");
            }
        }
        ret.Append("}");
        return ret.ToString();
    }

    public static string __ByOption(object value, PolyEvalType t) {
        if (value == null) {
            return "null";
        } else {
            return __ValToS(value, t.valueType);
        }
    }

    public static string __ValToS(object value, PolyEvalType t) {
        string typeName = t.typeName;
        if (typeName == "bool") {
            if (!(value is bool)) {
                throw new Exception("Type mismatch");
            }
            return __ByBool((bool)value);
        } else if (typeName == "int") {
            if (!(value is int) && !(value is double d && d == Math.Floor(d))) {
                throw new Exception("Type mismatch");
            }
            return __ByInt((int)value);
        } else if (typeName == "double") {
            if (!(value is int) && !(value is double)) {
                throw new Exception("Type mismatch");
            }
            return __ByDouble((double)value);
        } else if (typeName == "str") {
            if (!(value is string)) {
                throw new Exception("Type mismatch");
            }
            return __ByString((string)value);
        } else if (typeName == "list") {
            if (!(value is IList)) {
                throw new Exception("Type mismatch");
            }
            List<object> listValue = new List<object>();
            foreach (object v in (IList) value) {
                listValue.Add(v);
            }
            return __ByList(listValue, t);
        } else if (typeName == "ulist") {
            if (!(value is IList)) {
                throw new Exception("Type mismatch");
            }
            List<object> listValue = new List<object>();
            foreach (object v in (IList) value) {
                listValue.Add(v);
            }
            return __ByUlist(listValue, t);
        } else if (typeName == "dict") {
            if(!(value is IDictionary)) {
                throw new Exception("Type mismatch");
            }
            Dictionary<object, object> dictValue = new Dictionary<object, object>();
            foreach (DictionaryEntry kv in (IDictionary) value) {
                dictValue[kv.Key] = kv.Value;
            }
            return __ByDict(dictValue, t);
        } else if (typeName == "option") {
            return __ByOption(value, t);
        }
        throw new Exception($"Unknown type {typeName}");
    }

    public static string __Stringify(object value, string typeStr) {
        return __ValToS(value, __SToType(typeStr)) + ":" + typeStr;
    }
    
    public static void Main() {
        string tfs = __Stringify(true, "bool") + "\n";
        tfs += __Stringify(3, "int") + "\n";
        tfs += __Stringify(3.141592653, "double") + "\n";
        tfs += __Stringify("Hello, World!", "str") + "\n";
        tfs += __Stringify(new List<int> {1, 2, 3}, "list<int>") + "\n";
        tfs += __Stringify(new List<bool> {true, false, true}, "list<bool>") + "\n";
        tfs += __Stringify(new List<int> {3, 2, 1}, "ulist<int>") + "\n";
        tfs += __Stringify(new Dictionary<int, string> {{1, "one"}, {2, "two"}}, "dict<int,str>") + "\n";
        tfs += __Stringify(new Dictionary<string, List<int>> {{"one", new List<int> {1, 2, 3}}, {"two", new List<int> {4, 5, 6}}}, "dict<str,list<int>>") + "\n";
        tfs += __Stringify(null, "option<int>") + "\n";
        tfs += __Stringify(3, "option<int>") + "\n";
        System.IO.File.WriteAllText("stringify.out", tfs);
    }
}