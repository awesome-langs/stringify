class PolyEvalType {
    val typeStr: String
    val typeName: String
    val valueType: PolyEvalType?
    val keyType: PolyEvalType?

    constructor(typeStr: String, typeName: String, valueType: PolyEvalType?, keyType: PolyEvalType?) {
        this.typeStr = typeStr
        this.typeName = typeName
        this.valueType = valueType
        this.keyType = keyType
    }
}

fun sToType__(typeStr: String): PolyEvalType {
    if (!typeStr.contains("<")) {
        return PolyEvalType(typeStr, typeStr, null, null)
    } else {
        val idx = typeStr.indexOf("<")
        val typeName = typeStr.substring(0, idx)
        val otherStr = typeStr.substring(idx + 1, typeStr.length - 1)
        if (!otherStr.contains(",")) {
            val valueType = sToType__(otherStr)
            return PolyEvalType(typeStr, typeName, valueType, null)
        } else {
            val idx = otherStr.indexOf(",")
            val keyType = sToType__(otherStr.substring(0, idx))
            val valueType = sToType__(otherStr.substring(idx + 1))
            return PolyEvalType(typeStr, typeName, valueType, keyType)
        }
    }
}

fun escapeString__(s: String): String {
    val newS = StringBuilder()
    for (c in s) {
        when (c) {
            '\\' -> newS.append("\\\\")
            '\"' -> newS.append("\\\"")
            '\n' -> newS.append("\\n")
            '\t' -> newS.append("\\t")
            '\r' -> newS.append("\\r")
            else -> newS.append(c)
        }
    }
    return newS.toString()
}

fun byBool__(value: Boolean): String {
    return if (value) "true" else "false"
}

fun byInt__(value: Int): String {
    return value.toString()
}

fun byDouble__(value: Double): String {
    val vs = String.format("%.6f", value)
    var newVs = vs
    while (newVs.endsWith("0")) {
        newVs = newVs.substring(0, newVs.length - 1)
    }
    if (newVs.endsWith(".")) {
        newVs += "0"
    }
    if (newVs == "-0.0") {
        newVs = "0.0"
    }
    return newVs
}

fun byString__(value: String): String {
    return "\"" + escapeString__(value) + "\""
}

fun byList__(value: List<*>, ty: PolyEvalType): String {
    val vStrs = value.map { valToS__(it, ty.valueType!!) }
    return "[" + vStrs.joinToString(", ") + "]"
}

fun byUlist__(value: List<*>, ty: PolyEvalType): String {
    val vStrs = value.map { valToS__(it, ty.valueType!!) }
    return "[" + vStrs.sorted().joinToString(", ") + "]"
}

fun byDict__(value: Map<*, *>, ty: PolyEvalType): String {
    val vStrs = value.map { valToS__(it.key, ty.keyType!!) + "=>" + valToS__(it.value, ty.valueType!!) }
    return "{" + vStrs.sorted().joinToString(", ") + "}"
}

fun byOption__(value: Any?, ty: PolyEvalType): String {
    return if (value == null) "null" else valToS__(value, ty.valueType!!)
}

fun valToS__(value: Any?, ty: PolyEvalType): String {
    val typeName = ty.typeName
    return when (typeName) {
        "bool" -> if (value is Boolean) byBool__(value) else throw IllegalArgumentException("Type mismatch")
        "int" -> if (value is Int) byInt__(value) else throw IllegalArgumentException("Type mismatch")
        "double" -> if (value is Double) byDouble__(value) else throw IllegalArgumentException("Type mismatch")
        "str" -> if (value is String) byString__(value) else throw IllegalArgumentException("Type mismatch")
        "list" -> if (value is List<*>) byList__(value, ty) else throw IllegalArgumentException("Type mismatch")
        "ulist" -> if (value is List<*>) byUlist__(value, ty) else throw IllegalArgumentException("Type mismatch")
        "dict" -> if (value is Map<*, *>) byDict__(value, ty) else throw IllegalArgumentException("Type mismatch")
        "option" -> byOption__(value, ty)
        else -> throw IllegalArgumentException("Unknown type $typeName")
    }
}

fun stringify__(value: Any?, typeStr: String): String {
    return valToS__(value, sToType__(typeStr)) + ":" + typeStr
}

fun main() {
    val tfs = stringify__(true, "bool") + "\n" +
            stringify__(3, "int") + "\n" +
            stringify__(3.141592653, "double") + "\n" +
            stringify__(3.0, "double") + "\n" +
            stringify__("Hello, World!", "str") + "\n" +
            stringify__("!@#\$%^&*()\\\"\n\t", "str") + "\n" +
            stringify__(listOf(1, 2, 3), "list<int>") + "\n" +
            stringify__(listOf(true, false, true), "list<bool>") + "\n" +
            stringify__(listOf(3, 2, 1), "ulist<int>") + "\n" +
            stringify__(mapOf(1 to "one", 2 to "two"), "dict<int,str>") + "\n" +
            stringify__(mapOf("one" to listOf(1, 2, 3), "two" to listOf(4, 5, 6)), "dict<str,list<int>>") + "\n" +
            stringify__(null, "option<int>") + "\n" +
            stringify__(3, "option<int>") + "\n"
    val f = java.io.File("stringify.out")
    f.writeText(tfs)
}