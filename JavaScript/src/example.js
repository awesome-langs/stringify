class PolyEvalType {
    constructor(typeStr, typeName, valueType, keyType) {
        this.typeStr = typeStr;
        this.typeName = typeName;
        this.valueType = valueType;
        this.keyType = keyType;
    }
}

function __sToType(typeStr) {
    if (!typeStr.includes("<")) {
        return new PolyEvalType(typeStr, typeStr, null, null);
    } else {
        const idx = typeStr.indexOf("<");
        const typeName = typeStr.substring(0, idx);
        const otherStr = typeStr.substring(idx + 1, typeStr.length - 1);
        if (!otherStr.includes(",")) {
            const valueType = __sToType(otherStr);
            return new PolyEvalType(typeStr, typeName, valueType, null);
        } else {
            const idx = otherStr.indexOf(",");
            const keyType = __sToType(otherStr.substring(0, idx));
            const valueType = __sToType(otherStr.substring(idx + 1));
            return new PolyEvalType(typeStr, typeName, valueType, keyType);
        }
    }
}

function __escapeString(s) {
    let newS = [];
    for (let i = 0; i < s.length; i++) {
        const c = s[i];
        if (c === "\\") {
            newS.push("\\\\");
        } else if (c === "\"") {
            newS.push("\\\"");
        } else if (c === "\n") {
            newS.push("\\n");
        } else if (c === "\t") {
            newS.push("\\t");
        } else if (c === "\r") {
            newS.push("\\r");
        } else {
            newS.push(c);
        }
    }
    return newS.join("");
}

function __byBool(value) {
    return value ? "true" : "false";
}

function __byInt(value) {
    const v = parseInt(value);
    return v.toString();
}

function __byDouble(value) {
    const v = parseFloat(value);
    let vs = v.toFixed(6);
    while (vs.endsWith("0")) {
        vs = vs.substring(0, vs.length - 1);
    }
    if (vs.endsWith(".")) {
        vs += "0";
    }
    if (vs === "-0.0") {
        vs = "0.0";
    }
    return vs;
}

function __byString(value) {
    return '"' + __escapeString(value) + '"';
}

function __byList(value, t) {
    const vStrs = [];
    for (let i = 0; i < value.length; i++) {
        vStrs.push(__valToS(value[i], t.valueType));
    }
    let ret = "[";
    for (let i = 0; i < vStrs.length; i++) {
        ret += vStrs[i];
        if (i < vStrs.length - 1) {
            ret += ", ";
        }
    }
    ret += "]";
    return ret;
}

function __byUlist(value, t) {
    const vStrs = [];
    for (let i = 0; i < value.length; i++) {
        vStrs.push(__valToS(value[i], t.valueType));
    }
    vStrs.sort();
    let ret = "[";
    for (let i = 0; i < vStrs.length; i++) {
        ret += vStrs[i];
        if (i < vStrs.length - 1) {
            ret += ", ";
        }
    }
    ret += "]";
    return ret;
}

function __byDict(value, t) {
    const vStrs = [];
    for (let [key, val] of value) {
        vStrs.push(__valToS(key, t.keyType) + "=>" + __valToS(val, t.valueType));
    }
    vStrs.sort();
    let ret = "{";
    for (let i = 0; i < vStrs.length; i++) {
        ret += vStrs[i];
        if (i < vStrs.length - 1) {
            ret += ", ";
        }
    }
    ret += "}";
    return ret;
}

function __byOption(value, t) {
    if (value === null) {
        return "null";
    } else {
        return __valToS(value, t.valueType);
    }
}

function __valToS(value, t) {
    const typeName = t.typeName;
    if (typeName === "bool") {
        if (typeof value !== "boolean") {
            throw new Error("Type mismatch");
        }
        return __byBool(value);
    } else if (typeName === "int") {
        if (typeof value !== "number" && !Number.isInteger(value)) {
            throw new Error("Type mismatch");
        }
        return __byInt(value);
    } else if (typeName === "double") {
        if (typeof value !== "number") {
            throw new Error("Type mismatch");
        }
        return __byDouble(value);
    } else if (typeName === "str") {
        if (typeof value !== "string") {
            throw new Error("Type mismatch");
        }
        return __byString(value);
    } else if (typeName === "list") {
        if (!Array.isArray(value)) {
            throw new Error("Type mismatch");
        }
        return __byList(value, t);
    } else if (typeName === "ulist") {
        if (!Array.isArray(value)) {
            throw new Error("Type mismatch");
        }
        return __byUlist(value, t);
    } else if (typeName === "dict") {
        if (!(value instanceof Map)) {
            throw new Error("Type mismatch");
        }
        return __byDict(value, t);
    } else if (typeName === "option") {
        return __byOption(value, t);
    }
    throw new Error(`Unknown type ${typeName}`);
}

function __stringify(value, typeStr) {
    return __valToS(value, __sToType(typeStr)) + ":" + typeStr;
}

let tfs = __stringify(true, "bool") + "\n"
    + __stringify(3, "int") + "\n"
    + __stringify(3.141592653, "double") + "\n"
    + __stringify(3.0, "double") + "\n"
    + __stringify("!@#$%^&*()\n\t", "str") + "\n"
    + __stringify("Hello, World!", "str") + "\n"
    + __stringify([1, 2, 3], "list<int>") + "\n"
    + __stringify([1, 2, 3], "list<int>") + "\n"
    + __stringify([true, false, true], "list<bool>") + "\n"
    + __stringify([3, 2, 1], "ulist<int>") + "\n"
    + __stringify(new Map([[1, "one"], [2, "two"]]), "dict<int,str>") + "\n"
    + __stringify(new Map([["one", [1, 2, 3]], ["two", [4, 5, 6]]]), "dict<str,list<int>>") + "\n"
    + __stringify(null, "option<int>") + "\n"
    + __stringify(3, "option<int>") + "\n";
const fs = require("fs");
fs.writeFileSync("stringify.out", tfs);