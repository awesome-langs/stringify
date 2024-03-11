class PolyEvalType
    attr_accessor :type_str, :type_name, :value_type, :key_type
    
    def initialize(type_str, type_name, value_type, key_type)
        @type_str = type_str
        @type_name = type_name
        @value_type = value_type
        @key_type = key_type
    end
end

def __s_to_type(type_str)
    if not type_str.include? "<"
        return PolyEvalType.new(type_str, type_str, nil, nil)
    else
        idx = type_str.index "<"
        type_name = type_str[0...idx]
        other_str = type_str[idx + 1...-1]
        if not other_str.include? ","
            value_type = __s_to_type(other_str)
            return PolyEvalType.new(type_str, type_name, value_type, nil)
        else
            idx = other_str.index ","
            key_type = __s_to_type(other_str[0...idx])
            value_type = __s_to_type(other_str[idx + 1..-1])
            return PolyEvalType.new(type_str, type_name, value_type, key_type)
        end
    end
end

def __escape_string(s)
    new_s = s.chars.map do |c|
        if c == "\\"   
            "\\\\"
        elsif c == "\""
            "\\\""
        elsif c == "\n"
            "\\n"
        elsif c == "\t"
            "\\t"
        elsif c == "\r"
            "\\r"
        else
            c
        end
    end
    return new_s.join
end

def __by_bool(value)
    return value ? "true" : "false"
end

def __by_int(value)
    v = value.to_i
    return v.to_s
end

def __by_double(value)
    v = value.to_f
    vs = format("%.6f", v)
    while vs.end_with? "0"
        vs = vs[0...-1]
    end
    if vs.end_with? "."
        vs += "0"
    end
    if vs == "-0.0"
        vs = "0.0"
    end
    return vs
end

def __by_string(value)
    return '"' + __escape_string(value) + '"'
end

def __by_list(value, ty)
    v_strs = value.map do |v|
        __val_to_s(v, ty.value_type)
    end
    return "[" + v_strs.join(", ") + "]"
end

def __by_ulist(value, ty)
    v_strs = value.map do |v|
        __val_to_s(v, ty.value_type)
    end
    return "[" + v_strs.sort.join(", ") + "]"
end

def __by_dict(value, ty)
    v_strs = value.map do |key, val|
        __val_to_s(key, ty.key_type) + "=>" + __val_to_s(val, ty.value_type)
    end
    return "{" + v_strs.sort.join(", ") + "}"
end

def __by_option(value, ty)
    if value.nil?
        return "null"
    else
        return __val_to_s(value, ty.value_type)
    end
end

def __val_to_s(value, ty)
    type_name = ty.type_name
    if type_name == "bool"
        if not value.is_a? TrueClass and not value.is_a? FalseClass
            raise "Type mismatch"
        end
        return __by_bool(value)
    elsif type_name == "int"
        if not value.is_a? Integer and not (value.is_a? Float and value.to_i == value)
            raise "Type mismatch"
        end
        return __by_int(value)
    elsif type_name == "double"
        if not value.is_a? Integer and not value.is_a? Float
            raise "Type mismatch"
        end
        return __by_double(value)
    elsif type_name == "str"
        if not value.is_a? String
            raise "Type mismatch"
        end
        return __by_string(value)
    elsif type_name == "list"
        if not value.is_a? Array
            raise "Type mismatch"
        end
        return __by_list(value, ty)
    elsif type_name == "ulist"
        if not value.is_a? Array
            raise "Type mismatch"
        end
        return __by_ulist(value, ty)
    elsif type_name == "dict"
        if not value.is_a? Hash
            raise "Type mismatch"
        end
        return __by_dict(value, ty)
    elsif type_name == "option"
        return __by_option(value, ty)
    end
    raise "Unknown type #{type_name}"
end

def __stringify(value, type_str)
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