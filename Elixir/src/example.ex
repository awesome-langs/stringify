defmodule PolyEvalType do
    defstruct type_str: nil, type_name: nil, value_type: nil, key_type: nil
end

defmodule Example do
    def new_poly_eval_type__(type_str, type_name, value_type, key_type) do
        %PolyEvalType{type_str: type_str, type_name: type_name, value_type: value_type, key_type: key_type}
    end

    def s_to_type__(type_str) do
        if String.contains?(type_str, "<") do
            idx = :binary.match(type_str, "<") |> elem(0)
            type_name = String.slice(type_str, 0..(idx - 1))
            other_str = String.slice(type_str, (idx + 1)..-2)
            if String.contains?(other_str, ",") do
                idx = :binary.match(other_str, ",") |> elem(0)
                key_type = s_to_type__(String.slice(other_str, 0..(idx - 1)))
                value_type = s_to_type__(String.slice(other_str, (idx + 1)..-1))
                new_poly_eval_type__(type_str, type_name, value_type, key_type)
            else
                value_type = s_to_type__(other_str)
                new_poly_eval_type__(type_str, type_name, value_type, nil)
            end
        else
            new_poly_eval_type__(type_str, type_str, nil, nil)
        end
    end
    
    def escape_string__(s) do
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

    def by_bool__(value) do
        if value, do: "true", else: "false"
    end

    def by_int__(value) do
        Integer.to_string(value)
    end

    def by_double__(value) do
        vs = :erlang.float_to_binary(value, decimals: 6)
        vs = String.replace(vs, ~r"0+$", "")
        vs = String.replace(vs, ~r"\.$", ".0")
        if vs == "-0.0", do: "0.0", else: vs
    end

    def by_string__(value) do
        "\"" <> escape_string__(value) <> "\""
    end

    def by_list__(value, ty) do
        v_strs = Enum.map(value, fn v -> val_to_s__(v, ty.value_type) end)
        "[" <> Enum.join(v_strs, ", ") <> "]"
    end

    def by_ulist__(value, ty) do
        v_strs = Enum.map(value, fn v -> val_to_s__(v, ty.value_type) end)
        "[" <> Enum.join(Enum.sort(v_strs), ", ") <> "]"
    end

    def by_dict__(value, ty) do
        v_strs = Enum.map(value, fn {key, val} -> val_to_s__(key, ty.key_type) <> "=>" <> val_to_s__(val, ty.value_type) end)
        "{" <> Enum.join(Enum.sort(v_strs), ", ") <> "}"
    end
    
    def by_option__(value, ty) do
        if value == nil do
            "null"
        else
            val_to_s__(value, ty.value_type)
        end
    end

    def val_to_s__(value, ty) do
        type_name = ty.type_name
        case type_name do
            "bool" -> if is_boolean(value), do: by_bool__(value), else: raise "Type mismatch"
            "int" -> if is_integer(value), do: by_int__(value), else: raise "Type mismatch"
            "double" -> if is_float(value), do: by_double__(value), else: raise "Type mismatch"
            "str" -> if is_binary(value), do: by_string__(value), else: raise "Type mismatch"
            "list" -> if is_list(value), do: by_list__(value, ty), else: raise "Type mismatch"
            "ulist" -> if is_list(value), do: by_ulist__(value, ty), else: raise "Type mismatch"
            "dict" -> if is_map(value), do: by_dict__(value, ty), else: raise "Type mismatch"
            "option" -> by_option__(value, ty)
            _ -> raise "Unknown type #{type_name}"
        end
    end

    def stringify__(value, type_str) do
        val_to_s__(value, s_to_type__(type_str)) <> ":" <> type_str
    end

    def main() do
        tfs = stringify__(true, "bool") <> "\n" <>
            stringify__(3, "int") <> "\n" <>
            stringify__(3.141592653, "double") <> "\n" <>
            stringify__(3.0, "double") <> "\n" <>
            stringify__("Hello, World!", "str") <> "\n" <>
            stringify__("!@#$%^&*()\\\"\n\t", "str") <> "\n" <>
            stringify__([1, 2, 3], "list<int>") <> "\n" <>
            stringify__([true, false, true], "list<bool>") <> "\n" <>
            stringify__([3, 2, 1], "ulist<int>") <> "\n" <>
            stringify__(%{1 => "one", 2 => "two"}, "dict<int,str>") <> "\n" <>
            stringify__(%{"one" => [1, 2, 3], "two" => [4, 5, 6]}, "dict<str,list<int>>") <> "\n" <>
            stringify__(nil, "option<int>") <> "\n" <>
            stringify__(3, "option<int>") <> "\n"
        File.write!("stringify.out", tfs)
    end
end

Example.main()