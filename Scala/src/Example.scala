object Example {
    def myStringToInt(s: String): Int = {
        s.toInt
    }

    def myStringToDouble(s: String): Double = { 
        s.toDouble
    }

    def myIntToString(i: Int): String = {
        i.toString
    }

    def myDoubleToString(d: Double): String = { 
        f"$d%.6f"
    }

    def myBoolToString(b: Boolean): String = {
        if (b) "true" else "false"
    }

    def myIntToNullable(i: Int): Option[Int] = {
        if (i > 0) {
            Some(i)
        } else if (i < 0) {
            Some(-i)
        } else {
            None
        }
    }

    def myNullableToInt(i: Option[Int]): Int = {
        i.getOrElse(-1)
    }

    def myListSorted(lst: Seq[String]): Seq[String] = {
        lst.sorted
    }

    def myListSortedByLength(lst: Seq[String]): Seq[String] = {
        lst.sortBy(_.length)
    }

    def myListFilter(lst: Seq[Int]): Seq[Int] = {
        lst.filter(x => x % 3 == 0)
    }

    def myListMap(lst: Seq[Int]): Seq[Int] = {
        lst.map(x => x * x)
    }

    def myListReduce(lst: Seq[Int]): Int = {
        lst.foldLeft(0)((acc, x) => acc * 10 + x)
    }

    def myListOperations(lst: Seq[Int]): Int = {
        lst.filter(x => x % 3 == 0)
            .map(x => x * x)
            .foldLeft(0)((acc, x) => acc * 10 + x)
    }

    def myListToDict(lst: Seq[Int]): Map[Int, Int] = {
        lst.map(x => (x, x * x)).toMap
    }

    def myDictToList(dict: Map[Int, Int]): Seq[Int] = {
        dict.toList.sortBy(_._1).map(x => x._1 + x._2)
    }

    def myPrintString(s: String): Unit = {
        println(s)
    }

    def myPrintStringList(lst: Seq[String]): Unit = {
        for (x <- lst) {
            print(x + " ")
        }
        println()
    }

    def myPrintIntList(lst: Seq[Int]): Unit = {
        myPrintStringList(lst.map(x => myIntToString(x)))
    }

    def myPrintDict(dict: Map[Int, Int]): Unit = {
        for ((k, v) <- dict) {
            print(myIntToString(k) + "->" + myIntToString(v) + " ")
        }
        println()
    }

    def main(args: Array[String]): Unit = {
        myPrintString("Hello, World!")
        myPrintString(myIntToString(myStringToInt("123")))
        myPrintString(myDoubleToString(myStringToDouble("123.456")))
        myPrintString(myBoolToString(false))
        myPrintString(myIntToString(myNullableToInt(myIntToNullable(18))))
        myPrintString(myIntToString(myNullableToInt(myIntToNullable(-15))))
        myPrintString(myIntToString(myNullableToInt(myIntToNullable(0))))
        myPrintStringList(myListSorted(List("e", "dddd", "ccccc", "bb", "aaa")))
        myPrintStringList(myListSortedByLength(List("e", "dddd", "ccccc", "bb", "aaa")))
        myPrintString(myIntToString(myListReduce(myListMap(myListFilter(List(3, 12, 5, 8, 9, 15, 7, 17, 21, 11))))))
        myPrintString(myIntToString(myListOperations(List(3, 12, 5, 8, 9, 15, 7, 17, 21, 11))))
        myPrintDict(myListToDict(List(3, 1, 4, 2, 5, 9, 8, 6, 7, 0)))
        myPrintIntList(myDictToList(Map(3 -> 9, 1 -> 1, 4 -> 16, 2 -> 4, 5 -> 25, 9 -> 81, 8 -> 64, 6 -> 36, 7 -> 49, 0 -> 0)))
    }
}
