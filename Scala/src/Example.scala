class PolyEvalType {
    var typeStr: String = _
    var typeName: String = _
    var valueType: PolyEvalType = _
    var keyType: PolyEvalType = _
    
    def this(typeStr: String, typeName: String, valueType: PolyEvalType, keyType: PolyEvalType) = {
        this()
        this.typeStr = typeStr
        this.typeName = typeName
        this.valueType = valueType
        this.keyType = keyType
    }
}

object Example {
    def sToType__(typeStr: String): PolyEvalType = {
        if (typeStr.contains("<")) {
            val idx = typeStr.indexOf("<")
            val typeName = typeStr.substring(0, idx)
            val otherStr = typeStr.substring(idx + 1, typeStr.length - 1)
            if (otherStr.contains(",")) {
                val idx = otherStr.indexOf(",")
                val keyType = sToType__(otherStr.substring(0, idx))
                val valueType = sToType__(otherStr.substring(idx + 1))
                new PolyEvalType(typeStr, typeName, valueType, keyType)
            } else {
                val valueType = sToType__(otherStr)
                new PolyEvalType(typeStr, typeName, valueType, null)
            }
        } else {
            new PolyEvalType(typeStr, typeStr, null, null)
        }
    }

    def escapeString__(s: String): String = {
        val newS = new StringBuilder
        for (c <- s) {
            c match {
                case '\\' => newS.append("\\\\")
                case '\"' => newS.append("\\\"")
                case '\n' => newS.append("\\n")
                case '\t' => newS.append("\\t")
                case '\r' => newS.append("\\r")
                case _ => newS.append(c)
            }
        }
        newS.toString
    }

    def byBool__(value: Boolean): String = {
        if (value) "true" else "false"
    }

    def byInt__(value: Int): String = {
        value.toString
    }

    def byDouble__(value: Double): String = {
        val vs = f"$value%.6f"
        if (vs.endsWith("0")) {
            val vs_ = vs.reverse.dropWhile(_ == '0').reverse
            if (vs_.endsWith(".")) vs_ + "0" else vs_
        } else if (vs == "-0.0") "0.0" else vs
    }

    def byString__(value: String): String = {
        "\"" + escapeString__(value) + "\""
    }

    def byList__[A](value: List[A], ty: PolyEvalType): String = {
        val vStrs = value.map(v => valToS__(v, ty.valueType))
        "[" + vStrs.mkString(", ") + "]"
    }

    def byUList__[A](value: List[A], ty: PolyEvalType): String = {
        val vStrs = value.map(v => valToS__(v, ty.valueType))
        "[" + vStrs.sorted.mkString(", ") + "]"
    }

    def byDict__[A, B](value: Map[A, B], ty: PolyEvalType): String = {
        val vStrs = value.map { case (key, value) => valToS__(key, ty.keyType) + "=>" + valToS__(value, ty.valueType) }
        "{" + vStrs.toList.sorted.mkString(", ") + "}"
    }

    def byOption__[A](value: Option[A], ty: PolyEvalType): String = {
        if (value.isEmpty) "null" else valToS__(value.get, ty.valueType)
    }

    def valToS__[A](value: A, ty: PolyEvalType): String = {
        val typeName = ty.typeName
        typeName match {
            case "bool" => if (value.isInstanceOf[Boolean]) byBool__(value.asInstanceOf[Boolean]) else throw new IllegalArgumentException("Type mismatch")
            case "int" => if (value.isInstanceOf[Int]) byInt__(value.asInstanceOf[Int]) else throw new IllegalArgumentException("Type mismatch")
            case "double" => if (value.isInstanceOf[Double]) byDouble__(value.asInstanceOf[Double]) else throw new IllegalArgumentException("Type mismatch")
            case "str" => if (value.isInstanceOf[String]) byString__(value.asInstanceOf[String]) else throw new IllegalArgumentException("Type mismatch")
            case "list" => if (value.isInstanceOf[List[_]]) byList__(value.asInstanceOf[List[_]], ty) else throw new IllegalArgumentException("Type mismatch")
            case "ulist" => if (value.isInstanceOf[List[_]]) byUList__(value.asInstanceOf[List[_]], ty) else throw new IllegalArgumentException("Type mismatch")
            case "dict" => if (value.isInstanceOf[Map[_, _]]) byDict__(value.asInstanceOf[Map[_, _]], ty) else throw new IllegalArgumentException("Type mismatch")
            case "option" => byOption__(value.asInstanceOf[Option[_]], ty)
            case _ => throw new IllegalArgumentException(s"Unknown type $typeName")
        }
    }

    def stringify__[A](value: A, typeStr: String): String = {
        valToS__(value, sToType__(typeStr)) + ":" + typeStr
    }

    def main(args: Array[String]): Unit = {
        val tfs = stringify__(true, "bool") + "\n" +
            stringify__(3, "int") + "\n" +
            stringify__(3.141592653, "double") + "\n" +
            stringify__(3.0, "double") + "\n" +
            stringify__("Hello, World!", "str") + "\n" +
            stringify__("!@#$%^&*()\\\"\n\t", "str") + "\n" +
            stringify__(List(1, 2, 3), "list<int>") + "\n" +
            stringify__(List(true, false, true), "list<bool>") + "\n" +
            stringify__(List(3, 2, 1), "ulist<int>") + "\n" +
            stringify__(Map(1 -> "one", 2 -> "two"), "dict<int,str>") + "\n" +
            stringify__(Map("one" -> List(1, 2, 3), "two" -> List(4, 5, 6)), "dict<str,list<int>>") + "\n" +
            stringify__(None, "option<int>") + "\n" +
            stringify__(Some(3), "option<int>") + "\n"
        val fw = new java.io.FileWriter("stringify.out")
        fw.write(tfs)
        fw.close()
    }
}