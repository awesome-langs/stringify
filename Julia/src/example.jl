using Printf

struct PolyEvalType
    type_str::String
    type_name::String
    value_type::Union{PolyEvalType, Missing}
    key_type::Union{PolyEvalType, Missing}
end

function s_to_type__(type_str::String)::PolyEvalType
    if !occursin("<", type_str)
        return PolyEvalType(type_str, type_str, missing, missing)
    else
        idx = findfirst("<", type_str).start
        type_name = type_str[1:idx - 1]
        other_str = type_str[idx + 1:end - 1]
        if !occursin(",", other_str)
            value_type = s_to_type__(other_str)
            return PolyEvalType(type_str, type_name, value_type, missing)
        else
            idx = findfirst(",", other_str).start
            key_type = s_to_type__(other_str[1:idx - 1])
            value_type = s_to_type__(other_str[idx + 1:end])
            return PolyEvalType(type_str, type_name, value_type, key_type)
        end
    end
end

function escape_string__(s::String)::String
    new_s = ""
    for c in s
        if c == '\\'
            new_s *= "\\\\"
        elseif c == '"'
            new_s *= "\\\""
        elseif c == '\n'
            new_s *= "\\n"
        elseif c == '\t'
            new_s *= "\\t"
        elseif c == '\r'
            new_s *= "\\r"
        else
            new_s *= c
        end
    end
    return new_s
end

function by_bool__(value::Bool)::String
    return value ? "true" : "false"
end

function by_int__(value::Int)::String
    return string(value)
end

function by_double__(value::Float64)::String
    vs = @sprintf("%.6f", value)
    while endswith(vs, "0")
        vs = vs[1:end-1]
    end
    if endswith(vs, ".")
        vs *= "0"
    end
    if vs == "-0.0"
        vs = "0.0"
    end
    return vs
end

function by_string__(value::String)::String
    return '"' * escape_string__(value) * '"'
end

function by_list__(value::Vector, ty::PolyEvalType)::String
    v_strs = [val_to_s__(v, ty.value_type) for v in value]
    return "[" * join(v_strs, ", ") * "]"
end

function by_ulist__(value::Vector, ty::PolyEvalType)::String
    v_strs = [val_to_s__(v, ty.value_type) for v in value]
    return "[" * join(sort(v_strs), ", ") * "]"
end

function by_dict__(value::Dict, ty::PolyEvalType)::String
    v_strs = [val_to_s__(key, ty.key_type) * "=>" * val_to_s__(val, ty.value_type) for (key, val) in value]
    return "{" * join(sort(v_strs), ", ") * "}"
end

function by_option__(value, ty::PolyEvalType)::String
    if ismissing(value)
        return "null"
    else
        return val_to_s__(value, ty.value_type)
    end
end

function val_to_s__(value, ty::PolyEvalType)::String
    type_name = ty.type_name
    if type_name == "bool"
        if !isa(value, Bool)
            throw(ArgumentError("Type mismatch"))
        end
        return by_bool__(value)
    elseif type_name == "int"
        if !isa(value, Int)
            throw(ArgumentError("Type mismatch"))
        end
        return by_int__(value)
    elseif type_name == "double"
        if !isa(value, Float64)
            throw(ArgumentError("Type mismatch"))
        end
        return by_double__(value)
    elseif type_name == "str"
        if !isa(value, String)
            throw(ArgumentError("Type mismatch"))
        end
        return by_string__(value)
    elseif type_name == "list"
        if !isa(value, Vector)
            throw(ArgumentError("Type mismatch"))
        end
        return by_list__(value, ty)
    elseif type_name == "ulist"
        if !isa(value, Vector)
            throw(ArgumentError("Type mismatch"))
        end
        return by_ulist__(value, ty)
    elseif type_name == "dict"
        if !isa(value, Dict)
            throw(ArgumentError("Type mismatch"))
        end
        return by_dict__(value, ty)
    elseif type_name == "option"
        return by_option__(value, ty)
    end
    throw(ArgumentError("Unknown type $type_name"))
end

function stringify__(value, type_str::String)::String
    return val_to_s__(value, s_to_type__(type_str)) * ":" * type_str
end

tfs = stringify__(true, "bool") * "\n" *
    stringify__(3, "int") * "\n" *
    stringify__(3.141592653, "double") * "\n" *
    stringify__(3.0, "double") * "\n" *
    stringify__("Hello, World!", "str") * "\n" *
    stringify__("!@#\$%^&*()\\\"\n\t", "str") * "\n" *
    stringify__([1, 2, 3], "list<int>") * "\n" *
    stringify__([true, false, true], "list<bool>") * "\n" *
    stringify__([3, 2, 1], "ulist<int>") * "\n" *
    stringify__(Dict(1 => "one", 2 => "two"), "dict<int,str>") * "\n" *
    stringify__(Dict("one" => [1, 2, 3], "two" => [4, 5, 6]), "dict<str,list<int>>") * "\n" *
    stringify__(missing, "option<int>") * "\n" *
    stringify__(3, "option<int>") * "\n"

open("stringify.out", "w") do f
    write(f, tfs)
end