class PolyEvalType
    property type_str : String
    property type_name : String
    property value_type : PolyEvalType?
    property key_type : PolyEvalType?
    
    def initialize(type_str : String, type_name : String, value_type : PolyEvalType?, key_type : PolyEvalType?)
        @type_str = type_str
        @type_name = type_name
        @value_type = value_type
        @key_type = key_type
    end
end

def __s_to_type(type_str : String) : PolyEvalType
    if !type_str.includes? "<"
        return PolyEvalType.new(type_str, type_str, nil, nil)
    else
        idx = type_str.index("<").not_nil!
        type_name = type_str[0, idx]
        other_str = type_str[idx + 1, type_str.size - idx - 2]
        if !other_str.includes? ","
            value_type = __s_to_type(other_str)
            return PolyEvalType.new(type_str, type_name, value_type, nil)
        else
            idx = other_str.index(",").not_nil!
            key_type = __s_to_type(other_str[0, idx])
            value_type = __s_to_type(other_str[idx + 1, other_str.size - idx - 1])
            return PolyEvalType.new(type_str, type_name, value_type, key_type)
        end
    end
end

def __escape_string(s : String) : String
    new_s = s.chars.map do |c|
        if c == '\\'   
            "\\\\"
        elsif c == '"'
            "\\\""
        elsif c == '\n'
            "\\n"
        elsif c == '\t'
            "\\t"
        elsif c == '\r'
            "\\r"
        else
            c.to_s
        end
    end
    return new_s.join
end

def __by_bool(value : Bool) : String
    return value ? "true" : "false"
end

def __by_int(value : Int32) : String
    v = value.to_i
    return v.to_s
end

def __by_double(value : Float64) : String
    v = value.to_f
    vs = v.to_s
    while vs.ends_with?("0")
        vs = vs[0, vs.size - 1]
    end
    if vs.ends_with?(".")
        vs += "0"
    end
    if vs == "-0.0"
        vs = "0.0"
    end
    return vs
end

def __by_string(value : String) : String
    return '"' + __escape_string(value) + '"'
end

def __by_list(value : Array(_), ty : PolyEvalType) : String
    v_strs = value.map do |v|
        __val_to_s(v, ty.value_type.not_nil!)
    end
    return "[" + v_strs.join(", ") + "]"
end

def __by_ulist(value : Array(_), ty : PolyEvalType) : String
    v_strs = value.map do |v|
        __val_to_s(v, ty.value_type.not_nil!)
    end
    return "[" + v_strs.sort.join(", ") + "]"
end

def __by_dict(value : Hash(_, _), ty : PolyEvalType) : String
    v_strs = value.map do |key, val|
        __val_to_s(key, ty.key_type.not_nil!) + "=>" + __val_to_s(val, ty.value_type.not_nil!)
    end
    return "{" + v_strs.sort.join(", ") + "}"
end

def __by_option(value : Nil | _, ty : PolyEvalType) : String
    if value.nil?
        return "null"
    else
        return __val_to_s(value, ty.value_type.not_nil!)
    end
end

def __val_to_s(value : _, ty : PolyEvalType) : String
    type_name = ty.type_name
    if type_name == "bool"
        if !value.is_a?(Bool)
            raise "Type mismatch"
        end
        return __by_bool(value)
    elsif type_name == "int"
        if !value.is_a?(Int32)
           raise "Type mismatch" 
        end
        return __by_int(value)
    elsif type_name == "double"
        if !value.is_a?(Float64)
            raise "Type mismatch"
        end
        return __by_double(value)
    elsif type_name == "str"
        if !value.is_a?(String)
            raise "Type mismatch"
        end
        return __by_string(value)
    elsif type_name == "list"
        if !value.is_a?(Array)
            raise "Type mismatch"
        end
        return __by_list(value, ty)
    elsif type_name == "ulist"
        if !value.is_a?(Array)
            raise "Type mismatch"
        end
        return __by_ulist(value, ty)
    elsif type_name == "dict"
        if !value.is_a?(Hash)
            raise "Type mismatch"
        end
        return __by_dict(value, ty)
    elsif type_name == "option"
        return __by_option(value, ty)
    end
    raise "Unknown type #{type_name}"
end

def __stringify(value, type_str : String) : String
    return __val_to_s(value, __s_to_type(type_str)) + ":" + type_str
end

tfs = __stringify(true, "bool") + "\n" \
    + __stringify(3, "int") + "\n" \
    + __stringify(3.141592653, "double") + "\n" \
    + __stringify(3.0, "double") + "\n" \
    + __stringify("Hello, World!", "str") + "\n" \
    + __stringify("!@#$%^&*()\\\"\n\t", "str") + "\n" \
    + __stringify([1, 2, 3], "list<int>") + "\n" \
    + __stringify([true, false, true], "list<bool>") + "\n" \
    + __stringify([3, 2, 1], "ulist<int>") + "\n" \
    + __stringify({1 => "one", 2 => "two"}, "dict<int,str>") + "\n" \
    + __stringify({"one" => [1, 2, 3], "two" => [4, 5, 6]}, "dict<str,list<int>>") + "\n" \
    + __stringify(nil, "option<int>") + "\n" \
    + __stringify(3, "option<int>") + "\n"
File.write("stringify.out", tfs)