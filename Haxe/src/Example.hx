import de.polygonal.Printf;

class PolyEvalType {
    public var typeStr : String;
    public var typeName : String;
    public var valueType : Null<PolyEvalType>;
    public var keyType : Null<PolyEvalType>;

    public function new(typeStr : String, typeName : String, valueType : Null<PolyEvalType>, keyType : Null<PolyEvalType>) {
        this.typeStr = typeStr;
        this.typeName = typeName;
        this.valueType = valueType;
        this.keyType = keyType;
    }
}

class Example {
    public static function sToType__ (typeStr : String) : PolyEvalType {
        if (typeStr.indexOf("<") == -1) {
            return new PolyEvalType(typeStr, typeStr, null, null);
        } else {
            var idx = typeStr.indexOf("<");
            var typeName = typeStr.substring(0, idx);
            var otherStr = typeStr.substring(idx + 1, typeStr.length - 1);
            if (otherStr.indexOf(",") == -1) {
                var valueType = sToType__(otherStr);
                return new PolyEvalType(typeStr, typeName, valueType, null);
            } else {
                idx = otherStr.indexOf(",");
                var keyType = sToType__(otherStr.substring(0, idx));
                var valueType = sToType__(otherStr.substring(idx + 1));
                return new PolyEvalType(typeStr, typeName, valueType, keyType);
            }
        }
    }

    public static function escapeString__ (s : String) : String {
        var newS = new StringBuf();
        for (c in s.split("")) {
            switch (c) {
                case "\\":
                    newS.add("\\\\");
                case "\"":
                    newS.add("\\\"");
                case "\n":
                    newS.add("\\n");
                case "\t":
                    newS.add("\\t");
                case "\r":
                    newS.add("\\r");
                default:
                    newS.add(c);
            }
        }
        return newS.toString();
    }

    public static function byBool__ (value : Bool) : String {
        return value ? "true" : "false";
    }

    public static function byInt__ (value : Int) : String {
        return Std.string(value);
    }

    public static function byDouble__ (value : Float) : String {
        var v = Printf.format("%.6f", [value]);
        var vs = Std.string(v);
        while (vs.charAt(vs.length - 1) == "0") {
            vs = vs.substring(0, vs.length - 1);
        }
        if (vs.charAt(vs.length - 1) == ".") {
            vs += "0";
        }
        if (vs == "-0.0") {
            vs = "0.0";
        }
        return vs;
    }

    public static function byString__ (value : String) : String {
        return "\"" + escapeString__(value) + "\"";
    }

    public static function byList__ (value : Array<Dynamic>, ty : PolyEvalType) : String {
        var vStrs = [for (v in value) valToS__(v, ty.valueType)];
        return "[" + vStrs.join(", ") + "]";
    }

    public static function byUlist__ (value : Array<Dynamic>, ty : PolyEvalType) : String {
        var vStrs = [for (v in value) valToS__(v, ty.valueType)];
        vStrs.sort(Reflect.compare);
        return "[" + vStrs.join(", ") + "]";
    }

    public static function byDict__ (value : Map<Dynamic, Dynamic>, ty : PolyEvalType) : String {
        var vStrs = [for (k => v in value) valToS__(k, ty.keyType) + "=>" + valToS__(v, ty.valueType)];
        vStrs.sort(Reflect.compare);
        return "{" + vStrs.join(", ") + "}";
    }

    public static function byOption__ (value : Null<Dynamic>, ty : PolyEvalType) : String {
        if (value == null) {
            return "null";
        } else {
            return valToS__(value, ty.valueType);
        }
    }

    public static function valToS__ (value : Dynamic, ty : PolyEvalType) : String {
        var typeName = ty.typeName;
        switch (typeName) {
            case "bool":
                if (value is Bool) {
                    return byBool__(value);
                } else {
                    throw "Type mismatch";
                }
            case "int":
                if (value is Int) {
                    return byInt__(value);
                } else {
                    throw "Type mismatch";
                }
            case "double":
                if (value is Float) {
                    return byDouble__(value);
                } else {
                    throw "Type mismatch";
                }
            case "str":
                if (value is String) {
                    return byString__(value);
                } else {
                    throw "Type mismatch";
                }
            case "list":
                if (value is Array) {
                    return byList__(value, ty);
                } else {
                    throw "Type mismatch";
                }
            case "ulist":
                if (value is Array) {
                    return byUlist__(value, ty);
                } else {
                    throw "Type mismatch";
                }
            case "dict":
                if (Type.getClassName(Type.getClass(value)) == "haxe.ds.IntMap" || 
                    Type.getClassName(Type.getClass(value)) == "haxe.ds.StringMap") {
                    return byDict__(value, ty);
                } else {
                    throw "Type mismatch";
                }
            case "option":
                return byOption__(value, ty);
            default:
                throw "Unknown type " + typeName;
        }
    }

    public static function stringify__ (value : Dynamic, typeStr : String) : String {
        return valToS__(value, sToType__(typeStr)) + ":" + typeStr;
    }

    public static function main() {
        var tfs = stringify__(true, "bool") + "\n" +
            stringify__(3, "int") + "\n" +
            stringify__(3.141592653, "double") + "\n" +
            stringify__(3.0, "double") + "\n" +
            stringify__("Hello, World!", "str") + "\n" +
            stringify__("!@#$%^&*()\\\"\n\t", "str") + "\n" +
            stringify__([1, 2, 3], "list<int>") + "\n" +
            stringify__([true, false, true], "list<bool>") + "\n" +
            stringify__([3, 2, 1], "ulist<int>") + "\n" +
            stringify__([1 => "one", 2 => "two"], "dict<int,str>") + "\n" +
            stringify__(["one" => [1, 2, 3], "two" => [4, 5, 6]], "dict<str,list<int>>") + "\n" +
            stringify__(null, "option<int>") + "\n" +
            stringify__(3, "option<int>") + "\n";
        sys.io.File.saveContent("stringify.out", tfs);
    }
}