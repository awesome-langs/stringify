import java.util.*;
import java.io.*;

class PolyEvalType {
    public String typeStr;
    public String typeName;
    public PolyEvalType valueType;
    public PolyEvalType keyType;

    public PolyEvalType(String typeStr, String typeName, PolyEvalType valueType, PolyEvalType keyType) {
        this.typeStr = typeStr;
        this.typeName = typeName;
        this.valueType = valueType;
        this.keyType = keyType;
    }
}

public class Example {
    public static PolyEvalType __sToType(String typeStr) {
        if (!typeStr.contains("<")) {
            return new PolyEvalType(typeStr, typeStr, null, null);
        } else {
            int idx = typeStr.indexOf("<");
            String typeName = typeStr.substring(0, idx);
            String otherStr = typeStr.substring(idx + 1, typeStr.length() - 1);
            if (!otherStr.contains(",")) {
                PolyEvalType valueType = __sToType(otherStr);
                return new PolyEvalType(typeStr, typeName, valueType, null);
            } else {
                idx = otherStr.indexOf(",");
                PolyEvalType keyType = __sToType(otherStr.substring(0, idx));
                PolyEvalType valueType = __sToType(otherStr.substring(idx + 1));
                return new PolyEvalType(typeStr, typeName, valueType, keyType);
            }
        }
    }

    public static String __escapeString(String s) {
        StringBuilder newS = new StringBuilder();
        for (char c : s.toCharArray()) {
            if (c == '\\') {
                newS.append("\\\\");
            } else if (c == '\"') {
                newS.append("\\\"");
            } else if (c == '\n') {
                newS.append("\\n");
            } else if (c == '\t') {
                newS.append("\\t");
            } else if (c == '\r') {
                newS.append("\\r");
            } else {
                newS.append(c);
            }
        }
        return newS.toString();
    }

    public static String __byBool(boolean value) {
        return value ? "true" : "false";
    }

    public static String __byInt(int value) {
        return Integer.toString(value);
    }

    public static String __byDouble(double value) {
        String vs = String.format("%.6f", value);
        while (vs.endsWith("0")) {
            vs = vs.substring(0, vs.length() - 1);
        }
        if (vs.endsWith(".")) {
            vs += "0";
        }
        if (vs.equals("-0.0")) {
            vs = "0.0";
        }
        return vs;
    }

    public static String __byString(String value) {
        return "\"" + __escapeString(value) + "\"";
    }

    public static String __byList(List<Object> value, PolyEvalType t) {
        List<String> vStrs = new ArrayList<>();
        for (Object v : value) {
            vStrs.add(__valToS(v, t.valueType));
        }
        StringBuilder ret = new StringBuilder("[");
        for (int i = 0; i < vStrs.size(); i++) {
            ret.append(vStrs.get(i));
            if (i < vStrs.size() - 1) {
                ret.append(", ");
            }
        }
        ret.append("]");
        return ret.toString();
    }

    public static String __byUlist(List<Object> value, PolyEvalType t) {
        List<String> vStrs = new ArrayList<>();
        for (Object v : value) {
            vStrs.add(__valToS(v, t.valueType));
        }
        Collections.sort(vStrs);
        StringBuilder ret = new StringBuilder("[");
        for (int i = 0; i < vStrs.size(); i++) {
            ret.append(vStrs.get(i));
            if (i < vStrs.size() - 1) {
                ret.append(", ");
            }
        }
        ret.append("]");
        return ret.toString();
    }

    public static String __byDict(Map<Object, Object> value, PolyEvalType t) {
        List<String> vStrs = new ArrayList<>();
        for (Map.Entry<Object, Object> entry : value.entrySet()) {
            vStrs.add(__valToS(entry.getKey(), t.keyType) + "=>" + __valToS(entry.getValue(), t.valueType));
        }
        vStrs.sort(Comparator.naturalOrder());
        StringBuilder ret = new StringBuilder("{");
        for (int i = 0; i < vStrs.size(); i++) {
            ret.append(vStrs.get(i));
            if (i < vStrs.size() - 1) {
                ret.append(", ");
            }
        }
        ret.append("}");
        return ret.toString();
    }

    public static String __byOption(Optional<Object> value, PolyEvalType t) {
        if (value.isEmpty()) {
            return "null";
        } else {
            return __valToS(value.get(), t.valueType);
        }
    }

    public static String __valToS(Object value, PolyEvalType t) {
        String typeName = t.typeName;
        if (typeName.equals("bool")) {
            if (value instanceof Boolean) {
                return __byBool((boolean)value);
            } else {
                throw new IllegalArgumentException("Type mismatch");
            }
        } else if (typeName.equals("int")) {
            if (value instanceof Integer) {
                return __byInt((int)value);
            } else {
                throw new IllegalArgumentException("Type mismatch");
            }
        } else if (typeName.equals("double")) {
            if (value instanceof Double) {
                return __byDouble((double)value);
            } else {
                throw new IllegalArgumentException("Type mismatch");
            }
        } else if (typeName.equals("str")) {
            if (value instanceof String) {
                return __byString((String)value);
            } else {
                throw new IllegalArgumentException("Type mismatch");
            }
        } else if (typeName.equals("list")) {
            if (value instanceof List) {
                return __byList((List<Object>)value, t);
            } else {
                throw new IllegalArgumentException("Type mismatch");
            }
        } else if (typeName.equals("ulist")) {
            if (value instanceof List) {
                return __byUlist((List<Object>)value, t);
            } else {
                throw new IllegalArgumentException("Type mismatch");
            }
        } else if (typeName.equals("dict")) {
            if (value instanceof Map) {
                return __byDict((Map<Object, Object>)value, t);
            } else {
                throw new IllegalArgumentException("Type mismatch");
            }
        } else if (typeName.equals("option")) {
            return __byOption((Optional<Object>)value, t);
        }
        throw new IllegalArgumentException("Unknown type " + typeName);
    }
    
    public static String __stringify(Object value, String typeStr) {
        return __valToS(value, __sToType(typeStr)) + ":" + typeStr;
    }

    public static void main(String[] args) {
        String tfs = __stringify(true, "bool") + "\n"
            + __stringify(3, "int") + "\n"
            + __stringify(3.141592653, "double") + "\n"
            + __stringify(3.0, "double") + "\n"
            + __stringify("Hello, World!", "str") + "\n"
            + __stringify("!@#$%^&*()\\\"\n\t", "str") + "\n"
            + __stringify(List.of(1, 2, 3), "list<int>") + "\n"
            + __stringify(List.of(true, false, true), "list<bool>") + "\n"
            + __stringify(List.of(3, 2, 1), "ulist<int>") + "\n"
            + __stringify(Map.of(1, "one", 2, "two"), "dict<int,str>") + "\n"
            + __stringify(Map.of("one", List.of(1, 2, 3), "two", List.of(4, 5, 6)), "dict<str,list<int>>") + "\n"
            + __stringify(Optional.ofNullable(null), "option<int>") + "\n"
            + __stringify(Optional.ofNullable(3), "option<int>") + "\n";
        try (FileWriter writer = new FileWriter("stringify.out")) {
            writer.write(tfs);
        } catch (IOException e) {
        }
    }
}