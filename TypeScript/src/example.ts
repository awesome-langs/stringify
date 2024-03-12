import * as fs from "fs";

class PolyEvalType {
    typeStr: string;
    typeName: string;
    valueType: PolyEvalType | null;
    keyType: PolyEvalType | null;

    constructor(typeStr, typeName, valueType, keyType) {
        this.typeStr = typeStr;
        this.typeName = typeName;
        this.valueType = valueType;
        this.keyType = keyType;
    }
}

function sToType__(typeStr) {
    if (!typeStr.includes("<")) {
        return new PolyEvalType(typeStr, typeStr, null, null);
    } else {
        const idx = typeStr.indexOf("<");
        const typeName = typeStr.substring(0, idx);
        const otherStr = typeStr.substring(idx + 1, typeStr.length - 1);
        if (!otherStr.includes(",")) {
            const valueType = sToType__(otherStr);
            return new PolyEvalType(typeStr, typeName, valueType, null);
        } else {
            const idx = otherStr.indexOf(",");
            const keyType = sToType__(otherStr.substring(0, idx));
            const valueType = sToType__(otherStr.substring(idx + 1));
            return new PolyEvalType(typeStr, typeName, valueType, keyType);
        }
    }
}

function escapeString__(s) {
    let newS = [];
    for (let c of s) {
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

function byBool__(value) {
    return value ? "true" : "false";
}

function byInt__(value) {
    const v = parseInt(value);
    return v.toString();
}

function byDouble__(value) {
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

function byString__(value) {
    return '"' + escapeString__(value) + '"';
}

function byList__(value, t) {
    const vStrs = value.map((v) => valToS__(v, t.valueType));
    return "[" + vStrs.join(", ") + "]";
}

function byUlist__(value, t) {
    const vStrs = value.map((v) => valToS__(v, t.valueType));
    return "[" + vStrs.sort().join(", ") + "]";
}

function byDict__(value, t) {
    const vStrs = [...value].map(([key, val]) => valToS__(key, t.keyType) + "=>" + valToS__(val, t.valueType));
    return "{" + vStrs.sort().join(", ") + "}";
}

function byOption__(value, t) {
    if (value === null) {
        return "null";
    } else {
        return valToS__(value, t.valueType);
    }
}

function valToS__(value, t) {
    const typeName = t.typeName;
    if (typeName === "bool") {
        if (typeof value !== "boolean") {
            throw new Error("Type mismatch");
        }
        return byBool__(value);
    } else if (typeName === "int") {
        if (typeof value !== "number" && !Number.isInteger(value)) {
            throw new Error("Type mismatch");
        }
        return byInt__(value);
    } else if (typeName === "double") {
        if (typeof value !== "number") {
            throw new Error("Type mismatch");
        }
        return byDouble__(value);
    } else if (typeName === "str") {
        if (typeof value !== "string") {
            throw new Error("Type mismatch");
        }
        return byString__(value);
    } else if (typeName === "list") {
        if (!Array.isArray(value)) {
            throw new Error("Type mismatch");
        }
        return byList__(value, t);
    } else if (typeName === "ulist") {
        if (!Array.isArray(value)) {
            throw new Error("Type mismatch");
        }
        return byUlist__(value, t);
    } else if (typeName === "dict") {
        if (!(value instanceof Map)) {
            throw new Error("Type mismatch");
        }
        return byDict__(value, t);
    } else if (typeName === "option") {
        return byOption__(value, t);
    }
    throw new Error(`Unknown type ${typeName}`);
}

function stringify__(value, typeStr) {
    return valToS__(value, sToType__(typeStr)) + ":" + typeStr;
}

let tfs = stringify__(true, "bool") + "\n"
    + stringify__(3, "int") + "\n"
    + stringify__(3.141592653, "double") + "\n"
    + stringify__(3.0, "double") + "\n"
    + stringify__("!@#$%^&*()\n\t", "str") + "\n"
    + stringify__("Hello, World!", "str") + "\n"
    + stringify__([1, 2, 3], "list<int>") + "\n"
    + stringify__([1, 2, 3], "list<int>") + "\n"
    + stringify__([true, false, true], "list<bool>") + "\n"
    + stringify__([3, 2, 1], "ulist<int>") + "\n"
    + stringify__(new Map([[1, "one"], [2, "two"]]), "dict<int,str>") + "\n"
    + stringify__(new Map([["one", [1, 2, 3]], ["two", [4, 5, 6]]]), "dict<str,list<int>>") + "\n"
    + stringify__(null, "option<int>") + "\n"
    + stringify__(3, "option<int>") + "\n";

fs.writeFileSync("stringify.out", tfs);