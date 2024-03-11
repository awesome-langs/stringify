defmodule PolyEvalType do
    defstruct type_str: nil, type_name: nil, value_type: nil, key_type: nil
end

defmodule Example do
    def __new_poly_eval_type(type_str, type_name, value_type, key_type) do
        %PolyEvalType{type_str: type_str, type_name: type_name, value_type: value_type, key_type: key_type}
    end

    def __s_to_type(type_str) do
        if String.contains?(type_str, "<") do
            idx = :binary.match(type_str, "<") |> elem(0)
            type_name = String.slice(type_str, 0..(idx - 1))
            other_str = String.slice(type_str, (idx + 1)..-2)
            if String.contains?(other_str, ",") do
                idx = :binary.match(other_str, ",") |> elem(0)
                key_type = __s_to_type(String.slice(other_str, 0..(idx - 1)))
                value_type = __s_to_type(String.slice(other_str, (idx + 1)..-1))
                __new_poly_eval_type(type_str, type_name, value_type, key_type)
            else
                value_type = __s_to_type(other_str)
                __new_poly_eval_type(type_str, type_name, value_type, nil)
            end
        else
            __new_poly_eval_type(type_str, type_str, nil, nil)
        end
    end
    
    def __escape_string(s) do
        new_s = for c <- String.codepoints(s) do
            case c do
                "\\" -> "\\\\"
                "\"" -> "\\\""
                "\n" -> "\\n"
                "\t" -> "\\t"
                "\r" -> "\\r"
                _ -> c
            end
        end
        Enum.join(new_s)
    end

    def __by_bool(value) do
        if value, do: "true", else: "false"
    end

    def __by_int(value) do
        Integer.to_string(value)
    end

    def __by_double(value) do
        vs = :erlang.float_to_binary(value, decimals: 6)
        vs = String.replace(vs, ~r"0+$", "")
        vs = String.replace(vs, ~r"\.$", ".0")
        if vs == "-0.0", do: "0.0", else: vs
    end

    def __by_string(value) do
        "\"" <> __escape_string(value) <> "\""
    end

    def __by_list(value, ty) do
        v_strs = Enum.map(value, fn v -> __val_to_s(v, ty.value_type) end)
        "[" <> Enum.join(v_strs, ", ") <> "]"
    end

    def __by_ulist(value, ty) do
        v_strs = Enum.map(value, fn v -> __val_to_s(v, ty.value_type) end)
        "[" <> Enum.join(Enum.sort(v_strs), ", ") <> "]"
    end

    def __by_dict(value, ty) do
        v_strs = Enum.map(value, fn {key, val} -> __val_to_s(key, ty.key_type) <> "=>" <> __val_to_s(val, ty.value_type) end)
        "{" <> Enum.join(Enum.sort(v_strs), ", ") <> "}"
    end
    
    def __by_option(value, ty) do
        if value == nil do
            "null"
        else
            __val_to_s(value, ty.value_type)
        end
    end

    def __val_to_s(value, ty) do
        type_name = ty.type_name
        case type_name do
            "bool" -> if is_boolean(value), do: __by_bool(value), else: raise "Type mismatch"
            "int" -> if is_integer(value), do: __by_int(value), else: raise "Type mismatch"
            "double" -> if is_float(value), do: __by_double(value), else: raise "Type mismatch"
            "str" -> if is_binary(value), do: __by_string(value), else: raise "Type mismatch"
            "list" -> if is_list(value), do: __by_list(value, ty), else: raise "Type mismatch"
            "ulist" -> if is_list(value), do: __by_ulist(value, ty), else: raise "Type mismatch"
            "dict" -> if is_map(value), do: __by_dict(value, ty), else: raise "Type mismatch"
            "option" -> __by_option(value, ty)
            _ -> raise "Unknown type #{type_name}"
        end
    end

    def __stringify(value, type_str) do
        __val_to_s(value, __s_to_type(type_str)) <> ":" <> type_str
    end

    def main() do
        tfs = __stringify(true, "bool") <> "\n" <>
            __stringify(3, "int") <> "\n" <>
            __stringify(3.141592653, "double") <> "\n" <>
            __stringify(3.0, "double") <> "\n" <>
            __stringify("Hello, World!", "str") <> "\n" <>
            __stringify("!@#$%^&*()\\\"\n\t", "str") <> "\n" <>
            __stringify([1, 2, 3], "list<int>") <> "\n" <>
            __stringify([true, false, true], "list<bool>") <> "\n" <>
            __stringify([3, 2, 1], "ulist<int>") <> "\n" <>
            __stringify(%{1 => "one", 2 => "two"}, "dict<int,str>") <> "\n" <>
            __stringify(%{"one" => [1, 2, 3], "two" => [4, 5, 6]}, "dict<str,list<int>>") <> "\n" <>
            __stringify(nil, "option<int>") <> "\n" <>
            __stringify(3, "option<int>") <> "\n"
        File.write!("__stringify.out", tfs)
    end
end

Example.main()