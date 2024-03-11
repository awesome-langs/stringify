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
    public static PolyEvalType sToType__(String typeStr) {
        if (!typeStr.contains("<")) {
            return new PolyEvalType(typeStr, typeStr, null, null);
        } else {
            int idx = typeStr.indexOf("<");
            String typeName = typeStr.substring(0, idx);
            String otherStr = typeStr.substring(idx + 1, typeStr.length() - 1);
            if (!otherStr.contains(",")) {
                PolyEvalType valueType = sToType__(otherStr);
                return new PolyEvalType(typeStr, typeName, valueType, null);
            } else {
                idx = otherStr.indexOf(",");
                PolyEvalType keyType = sToType__(otherStr.substring(0, idx));
                PolyEvalType valueType = sToType__(otherStr.substring(idx + 1));
                return new PolyEvalType(typeStr, typeName, valueType, keyType);
            }
        }
    }

    public static String escapeString__(String s) {
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

    public static String byBool__(boolean value) {
        return value ? "true" : "false";
    }

    public static String byInt__(int value) {
        return Integer.toString(value);
    }

    public static String byDouble__(double value) {
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

    public static String byString__(String value) {
        return "\"" + escapeString__(value) + "\"";
    }

    public static String byList__(List<Object> value, PolyEvalType t) {
        var vStrs = value.stream().map(v -> valToS__(v, t.valueType)).toList();
        return "[" + String.join(", ", vStrs) + "]";
    }

    public static String byUlist__(List<Object> value, PolyEvalType t) {
        var vStrs = value.stream().map(v -> valToS__(v, t.valueType)).sorted(Comparator.naturalOrder()).toList();
        return "[" + String.join(", ", vStrs) + "]";
    }

    public static String byDict__(Map<Object, Object> value, PolyEvalType t) {
        var vStrs = value.entrySet().stream().map(entry -> valToS__(entry.getKey(), t.keyType) + "=>" + valToS__(entry.getValue(), t.valueType)).sorted(Comparator.naturalOrder()).toList();
        return "{" + String.join(", ", vStrs) + "}";
    }

    public static String byOption__(Optional<Object> value, PolyEvalType t) {
        if (value.isEmpty()) {
            return "null";
        } else {
            return valToS__(value.get(), t.valueType);
        }
    }

    public static String valToS__(Object value, PolyEvalType t) {
        String typeName = t.typeName;
        if (typeName.equals("bool")) {
            if (value instanceof Boolean) {
                return byBool__((boolean)value);
            } else {
                throw new IllegalArgumentException("Type mismatch");
            }
        } else if (typeName.equals("int")) {
            if (value instanceof Integer) {
                return byInt__((int)value);
            } else {
                throw new IllegalArgumentException("Type mismatch");
            }
        } else if (typeName.equals("double")) {
            if (value instanceof Double) {
                return byDouble__((double)value);
            } else {
                throw new IllegalArgumentException("Type mismatch");
            }
        } else if (typeName.equals("str")) {
            if (value instanceof String) {
                return byString__((String)value);
            } else {
                throw new IllegalArgumentException("Type mismatch");
            }
        } else if (typeName.equals("list")) {
            if (value instanceof List) {
                return byList__((List<Object>)value, t);
            } else {
                throw new IllegalArgumentException("Type mismatch");
            }
        } else if (typeName.equals("ulist")) {
            if (value instanceof List) {
                return byUlist__((List<Object>)value, t);
            } else {
                throw new IllegalArgumentException("Type mismatch");
            }
        } else if (typeName.equals("dict")) {
            if (value instanceof Map) {
                return byDict__((Map<Object, Object>)value, t);
            } else {
                throw new IllegalArgumentException("Type mismatch");
            }
        } else if (typeName.equals("option")) {
            return byOption__((Optional<Object>)value, t);
        }
        throw new IllegalArgumentException("Unknown type " + typeName);
    }
    
    public static String stringify__(Object value, String typeStr) {
        return valToS__(value, sToType__(typeStr)) + ":" + typeStr;
    }

    public static void main(String[] args) {
        String tfs = stringify__(true, "bool") + "\n"
            + stringify__(3, "int") + "\n"
            + stringify__(3.141592653, "double") + "\n"
            + stringify__(3.0, "double") + "\n"
            + stringify__("Hello, World!", "str") + "\n"
            + stringify__("!@#$%^&*()\\\"\n\t", "str") + "\n"
            + stringify__(List.of(1, 2, 3), "list<int>") + "\n"
            + stringify__(List.of(true, false, true), "list<bool>") + "\n"
            + stringify__(List.of(3, 2, 1), "ulist<int>") + "\n"
            + stringify__(Map.of(1, "one", 2, "two"), "dict<int,str>") + "\n"
            + stringify__(Map.of("one", List.of(1, 2, 3), "two", List.of(4, 5, 6)), "dict<str,list<int>>") + "\n"
            + stringify__(Optional.ofNullable(null), "option<int>") + "\n"
            + stringify__(Optional.ofNullable(3), "option<int>") + "\n";
        try (FileWriter writer = new FileWriter("stringify.out")) {
            writer.write(tfs);
        } catch (IOException e) {
        }
    }
}