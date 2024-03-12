Module Example
    Public Class PolyEvalType
        Public Property type_str As String
        Public Property type_name As String
        Public Property value_type As PolyEvalType
        Public Property key_type As PolyEvalType
        Public Sub New(type_str As String, type_name As String, value_type As PolyEvalType, key_type As PolyEvalType)
            Me.type_str = type_str
            Me.type_name = type_name
            Me.value_type = value_type
            Me.key_type = key_type
        End Sub
    End Class

    Public Function s_to_type__(type_str As String) As PolyEvalType
        If Not type_str.Contains("<") Then
            Return New PolyEvalType(type_str, type_str, Nothing, Nothing)
        Else
            Dim idx As Integer = type_str.IndexOf("<")
            Dim type_name As String = type_str.Substring(0, idx)
            Dim other_str As String = type_str.Substring(idx + 1, type_str.Length - idx - 2)
            If Not other_str.Contains(",") Then
                Dim value_type As PolyEvalType = s_to_type__(other_str)
                Return New PolyEvalType(type_str, type_name, value_type, Nothing)
            Else
                idx = other_str.IndexOf(",")
                Dim key_type As PolyEvalType = s_to_type__(other_str.Substring(0, idx))
                Dim value_type As PolyEvalType = s_to_type__(other_str.Substring(idx + 1))
                Return New PolyEvalType(type_str, type_name, value_type, key_type)
            End If
        End If
    End Function

    Public Function escape_string__(s As String) As String
        Dim new_s As New List(Of String)
        For Each c As Char In s
            If c = "\" Then
                new_s.Add("\\")
            ElseIf c = """" Then
                new_s.Add("\""")
            ElseIf c = vbLf Then
                new_s.Add("\n")
            ElseIf c = vbTab Then
                new_s.Add("\t")
            ElseIf c = vbCr Then
                new_s.Add("\r")
            Else
                new_s.Add(c)
            End If
        Next
        Return String.Join("", new_s)
    End Function

    Public Function by_bool__(value As Boolean) As String
        Return If(value, "true", "false")
    End Function

    Public Function by_int__(value As Integer) As String
        Return value.ToString()
    End Function

    Public Function by_double__(value As Double) As String
        Dim vs As String = value.ToString("F6")
        While vs.EndsWith("0")
            vs = vs.Substring(0, vs.Length - 1)
        End While
        If vs.EndsWith(".") Then
            vs += "0"
        End If
        If vs = "-0.0" Then
            vs = "0.0"
        End If
        Return vs
    End Function

    Public Function by_string__(value As String) As String
        Return """" & escape_string__(value) & """"
    End Function

    Public Function by_list__(value As List(Of Object), ty As PolyEvalType) As String
        Dim v_strs As New List(Of String)
        For Each v As Object In value
            v_strs.Add(val_to_s__(v, ty.value_type))
        Next
        Return "[" & String.Join(", ", v_strs) & "]"
    End Function

    Public Function by_ulist__(value As List(Of Object), ty As PolyEvalType) As String
        Dim v_strs As New List(Of String)
        For Each v As Object In value
            v_strs.Add(val_to_s__(v, ty.value_type))
        Next
        v_strs.Sort()
        Return "[" & String.Join(", ", v_strs) & "]"
    End Function

    Public Function by_dict__(value As Dictionary(Of Object, Object), ty As PolyEvalType) As String
        Dim v_strs As New List(Of String)
        For Each kvp As KeyValuePair(Of Object, Object) In value
            v_strs.Add(val_to_s__(kvp.Key, ty.key_type) & "=>" & val_to_s__(kvp.Value, ty.value_type))
        Next
        v_strs.Sort()
        Return "{" & String.Join(", ", v_strs) & "}"
    End Function

    Public Function by_option__(value As Object, ty As PolyEvalType) As String
        If value Is Nothing Then
            Return "null"
        Else
            Return val_to_s__(value, ty.value_type)
        End If
    End Function

    Public Function val_to_s__(value As Object, ty As PolyEvalType) As String
        Dim type_name As String = ty.type_name
        If type_name = "bool" Then
            If Not TypeOf value Is Boolean Then
                Throw New Exception("Type mismatch")
            End If
            Return by_bool__(value)
        ElseIf type_name = "int" Then
            If Not TypeOf value Is Integer Then
                Throw New Exception("Type mismatch")
            End If
            Return by_int__(value)
        ElseIf type_name = "double" Then
            If Not TypeOf value Is Integer AndAlso Not TypeOf value Is Double Then
                Throw New Exception("Type mismatch")
            End If
            Return by_double__(value)
        ElseIf type_name = "str" Then
            If Not TypeOf value Is String Then
                Throw New Exception("Type mismatch")
            End If
            Return by_string__(value)
        ElseIf type_name = "list" Then
            If Not TypeOf value Is List(Of Object) Then
                Throw New Exception("Type mismatch")
            End If
            Return by_list__(value, ty)
        ElseIf type_name = "ulist" Then
            If Not TypeOf value Is List(Of Object) Then
                Throw New Exception("Type mismatch")
            End If
            Return by_ulist__(value, ty)
        ElseIf type_name = "dict" Then
            If Not TypeOf value Is Dictionary(Of Object, Object) Then
                Throw New Exception("Type mismatch")
            End If
            Return by_dict__(value, ty)
        ElseIf type_name = "option" Then
            Return by_option__(value, ty)
        End If
        Throw New Exception("Unknown type " & type_name)
    End Function

    Public Function stringify__(value As Object, type_str As String) As String
        Return val_to_s__(value, s_to_type__(type_str)) & ":" & type_str
    End Function

    Sub Main()
        Dim tfs As String = stringify__(True, "bool") & vbLf &
            stringify__(3, "int") & vbLf &
            stringify__(3.141592653, "double") & vbLf &
            stringify__(3.0, "double") & vbLf &
            stringify__("Hello, World!", "str") & vbLf &
            stringify__("!@#$%^&*()\" & """" & vbLf & vbTab, "str") & vbLf &
            stringify__(New List(Of Object) From {1, 2, 3}, "list<int>") & vbLf &
            stringify__(New List(Of Object) From {True, False, True}, "list<bool>") & vbLf &
            stringify__(New List(Of Object) From {3, 2, 1}, "ulist<int>") & vbLf &
            stringify__(New Dictionary(Of Object, Object) From {{1, "one"}, {2, "two"}}, "dict<int,str>") & vbLf &
            stringify__(New Dictionary(Of Object, Object) From {{"one", New List(Of Object) From {1, 2, 3}}, {"two", New List(Of Object) From {4, 5, 6}}}, "dict<str,list<int>>") & vbLf &
            stringify__(Nothing, "option<int>") & vbLf &
            stringify__(3, "option<int>") & vbLf
        System.IO.File.WriteAllText("stringify.out", tfs)
    End Sub
End Module