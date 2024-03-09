Module Example
    Public Function MyStringToInt(s As String) As Integer
        Return Integer.Parse(s)
    End Function

    Public Function MyStringToDouble(s As String) As Double
        Return Double.Parse(s)
    End Function

    Public Function MyIntToString(i As Integer) As String
        Return i.ToString()
    End Function

    Public Function MyDoubleToString(d As Double) As String
        Return d.ToString("N6")
    End Function

    Public Function MyBoolToString(b As Boolean) As String
        Return If(b, "true", "false")
    End Function

    Public Function MyIntToNullable(i As Integer) As Integer?
        If i > 0 Then
            Return i
        ElseIf i < 0 Then
            Return -i
        Else
            Return Nothing
        End If
    End Function

    Public Function MyNullableToInt(i As Integer?) As Integer
        Return If(i.HasValue, i.Value, -1)
    End Function

    Public Function MyListSorted(lst As IList(Of String)) As IList(Of String)
        Return lst.OrderBy(Function(x) x).ToList()
    End Function

    Public Function MyListSortedByLength(lst As IList(Of String)) As IList(Of String)
        Return lst.OrderBy(Function(x) x.Length).ToList()
    End Function

    Public Function MyListFilter(lst As IList(Of Integer)) As IList(Of Integer)
        Return lst.Where(Function(x) x Mod 3 = 0).ToList()
    End Function

    Public Function MyListMap(lst As IList(Of Integer)) As IList(Of Integer)
        Return lst.Select(Function(x) x * x).ToList()
    End Function

    Public Function MyListReduce(lst As IList(Of Integer)) As Integer
        Return lst.Aggregate(0, Function(acc, x) acc * 10 + x)
    End Function

    Public Function MyListOperations(lst As IList(Of Integer)) As Integer
        Return lst.Where(Function(x) x Mod 3 = 0).
            Select(Function(x) x * x).
            Aggregate(0, Function(acc, x) acc * 10 + x)
    End Function

    Public Function MyListToDict(lst As IList(Of Integer)) As IDictionary(Of Integer, Integer)
        Return lst.ToDictionary(Function(x) x, Function(x) x * x)
    End Function

    Public Function MyDictToList(dict As IDictionary(Of Integer, Integer)) As IList(Of Integer)
        Return dict.OrderBy(Function(x) x.Key).Select(Function(x) x.Key + x.Value).ToList()
    End Function

    Public Sub MyPrintString(s As String)
        Console.WriteLine(s)
    End Sub

    Public Sub MyPrintStringList(lst As IList(Of String))
        For Each x In lst
            Console.Write(x & " ")
        Next
        Console.WriteLine()
    End Sub

    Public Sub MyPrintIntList(lst As IList(Of Integer))
        MyPrintStringList(lst.Select(Function(x) MyIntToString(x)).ToList())
    End Sub

    Public Sub MyPrintDict(dict As IDictionary(Of Integer, Integer))
        For Each x In dict
            Console.Write(MyIntToString(x.Key) & "->" & MyIntToString(x.Value) & " ")
        Next
        Console.WriteLine()
    End Sub

    Public Sub Main()
        MyPrintString("Hello, World!")
        MyPrintString(MyIntToString(MyStringToInt("123")))
        MyPrintString(MyDoubleToString(MyStringToDouble("123.456")))
        MyPrintString(MyBoolToString(False))
        MyPrintString(MyIntToString(MyNullableToInt(MyIntToNullable(18))))
        MyPrintString(MyIntToString(MyNullableToInt(MyIntToNullable(-15))))
        MyPrintString(MyIntToString(MyNullableToInt(MyIntToNullable(0))))
        MyPrintStringList(MyListSorted(New List(Of String) From {"e", "dddd", "ccccc", "bb", "aaa"}))
        MyPrintStringList(MyListSortedByLength(New List(Of String) From {"e", "dddd", "ccccc", "bb", "aaa"}))
        MyPrintString(MyIntToString(MyListReduce(MyListMap(MyListFilter(New List(Of Integer) From {3, 12, 5, 8, 9, 15, 7, 17, 21, 11})))))
        MyPrintString(MyIntToString(MyListOperations(New List(Of Integer) From {3, 12, 5, 8, 9, 15, 7, 17, 21, 11})))
        MyPrintDict(MyListToDict(New List(Of Integer) From {3, 1, 4, 2, 5, 9, 8, 6, 7, 0}))
        MyPrintIntList(MyDictToList(New Dictionary(Of Integer, Integer) From {{3, 9}, {1, 1}, {4, 16}, {2, 4}, {5, 25}, {9, 81}, {8, 64}, {6, 36}, {7, 49}, {0, 0}}))
    End Sub
End Module