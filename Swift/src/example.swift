import Foundation

class PolyEvalType {
    var typeStr = ""
    var typeName = ""
    var valueType: PolyEvalType?
    var keyType: PolyEvalType?

    init(_ typeStr: String, _ typeName: String, _ valueType: PolyEvalType?, _ keyType: PolyEvalType?) {
        self.typeStr = typeStr
        self.typeName = typeName
        self.valueType = valueType
        self.keyType = keyType
    }
}

func sToType__(_ typeStr: String) -> PolyEvalType {
    if !typeStr.contains("<") {
        return PolyEvalType(typeStr, typeStr, nil, nil)
    } else {
        let idx = typeStr.firstIndex(of: "<")!
        let type_name = String(typeStr[..<idx])
        let other_str = String(typeStr[typeStr.index(after: idx)..<typeStr.index(before: typeStr.endIndex)])
        if !other_str.contains(",") {
            let value_type = sToType__(other_str)
            return PolyEvalType(typeStr, type_name, value_type, nil)
        } else {
            let idx = other_str.firstIndex(of: ",")!
            let key_type = sToType__(String(other_str[..<idx]))
            let value_type = sToType__(String(other_str[other_str.index(after: idx)...]))
            return PolyEvalType(typeStr, type_name, value_type, key_type)
        }
    }
}

func escapeString__(_ s: String) -> String {
    var new_s = ""
    for c in s {
        switch c {
        case "\\":
            new_s += "\\\\"
        case "\"":
            new_s += "\\\""
        case "\n":
            new_s += "\\n"
        case "\t":
            new_s += "\\t"
        case "\r":
            new_s += "\\r"
        default:
            new_s.append(c)
        }
    }
    return new_s
}

func byBool__(_ value: Bool) -> String {
    return value ? "true" : "false"
}

func byInt__(_ value: Int) -> String {
    return String(value)
}

func byDouble__(_ value: Double) -> String {
    return String(format: "%.6f", value)
}

func byString__(_ value: String) -> String {
    return "\"" + escapeString__(value) + "\""
}

func byList__(_ value: [Any], _ ty: PolyEvalType) -> String {
    let v_strs = value.map { val_to_s__($0, ty.valueType!) }
    return "[" + v_strs.joined(separator: ", ") + "]"
}

func byUlist__(_ value: [Any], _ ty: PolyEvalType) -> String {
    let v_strs = value.map { val_to_s__($0, ty.valueType!) }
    return "[" + v_strs.sorted().joined(separator: ", ") + "]"
}

func byDict__(_ value: [AnyHashable: Any], _ ty: PolyEvalType) -> String {
    let v_strs = value.map { key, val in val_to_s__(key, ty.keyType!) + "=>" + val_to_s__(val, ty.valueType!) }
    return "{" + v_strs.sorted().joined(separator: ", ") + "}"
}

func byOption__(_ value: Any?, _ ty: PolyEvalType) -> String {
    if value == nil {
        return "null"
    } else {
        return val_to_s__(value!, ty.valueType!)
    }
}

func val_to_s__(_ value: Any?, _ ty: PolyEvalType) -> String {
    let type_name = ty.typeName
    switch type_name {
    case "bool":
        if let value = value as? Bool {
            return byBool__(value)
        } else {
            fatalError("Type mismatch")
        }
    case "int":
        if let value = value as? Int {
            return byInt__(value)
        } else {
            fatalError("Type mismatch")
        }
    case "double":
        if let value = value as? Double {
            return byDouble__(value)
        } else {
            fatalError("Type mismatch")
        }
    case "str":
        if let value = value as? String {
            return byString__(value)
        } else {
            fatalError("Type mismatch")
        }
    case "list":
        if let value = value as? [Any] {
            return byList__(value, ty)
        } else {
            fatalError("Type mismatch")
        }
    case "ulist":
        if let value = value as? [Any] {
            return byUlist__(value, ty)
        } else {
            fatalError("Type mismatch")
        }
    case "dict":
        if let value = value as? [AnyHashable: Any] {
            return byDict__(value, ty)
        } else {
            fatalError("Type mismatch")
        }
    case "option":
        return byOption__(value, ty)
    default:
        fatalError("Unknown type \(type_name)")
    }
}

func stringify__(_ value: Any?, _ type_str: String) -> String {
    return val_to_s__(value, sToType__(type_str)) + ":" + type_str
}

let tfs = stringify__(true, "bool") + "\n" +
    stringify__(3, "int") + "\n" +
    stringify__(3.141592653, "double") + "\n" +
    stringify__(3.0, "double") + "\n" +
    stringify__("Hello, World!", "str") + "\n" +
    stringify__("!@#$%^&*()\\\"\n\t", "str") + "\n" +
    stringify__([1, 2, 3], "list<int>") + "\n" +
    stringify__([true, false, true], "list<bool>") + "\n" +
    stringify__([3, 2, 1], "ulist<int>") + "\n" +
    stringify__([1: "one", 2: "two"], "dict<int,str>") + "\n" +
    stringify__(["one": [1, 2, 3], "two": [4, 5, 6]], "dict<str,list<int>>") + "\n" +
    stringify__(nil, "option<int>") + "\n" +
    stringify__(3, "option<int>") + "\n"
try! tfs.write(toFile: "stringify.out", atomically: true, encoding: .utf8)