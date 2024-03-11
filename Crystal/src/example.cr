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

def s_to_type__(type_str : String) : PolyEvalType
    if !type_str.includes? "<"
        return PolyEvalType.new(type_str, type_str, nil, nil)
    else
        idx = type_str.index("<").not_nil!
        type_name = type_str[0, idx]
        other_str = type_str[idx + 1, type_str.size - idx - 2]
        if !other_str.includes? ","
            value_type = s_to_type__(other_str)
            return PolyEvalType.new(type_str, type_name, value_type, nil)
        else
            idx = other_str.index(",").not_nil!
            key_type = s_to_type__(other_str[0, idx])
            value_type = s_to_type__(other_str[idx + 1, other_str.size - idx - 1])
            return PolyEvalType.new(type_str, type_name, value_type, key_type)
        end
    end
end

def escape_string__(s : String) : String
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

def by_bool__(value : Bool) : String
    return value ? "true" : "false"
end

def by_int__(value : Int32) : String
    v = value.to_i
    return v.to_s
end

def by_double__(value : Float64) : String
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

def by_string__(value : String) : String
    return '"' + escape_string__(value) + '"'
end

def by_list__(value : Array(_), ty : PolyEvalType) : String
    v_strs = value.map do |v|
        val_to_s__(v, ty.value_type.not_nil!)
    end
    return "[" + v_strs.join(", ") + "]"
end

def by_ulist__(value : Array(_), ty : PolyEvalType) : String
    v_strs = value.map do |v|
        val_to_s__(v, ty.value_type.not_nil!)
    end
    return "[" + v_strs.sort.join(", ") + "]"
end

def by_dict__(value : Hash(_, _), ty : PolyEvalType) : String
    v_strs = value.map do |key, val|
        val_to_s__(key, ty.key_type.not_nil!) + "=>" + val_to_s__(val, ty.value_type.not_nil!)
    end
    return "{" + v_strs.sort.join(", ") + "}"
end

def by_option__(value : Nil | _, ty : PolyEvalType) : String
    if value.nil?
        return "null"
    else
        return val_to_s__(value, ty.value_type.not_nil!)
    end
end

def val_to_s__(value : _, ty : PolyEvalType) : String
    type_name = ty.type_name
    if type_name == "bool"
        if !value.is_a?(Bool)
            raise "Type mismatch"
        end
        return by_bool__(value)
    elsif type_name == "int"
        if !value.is_a?(Int32)
           raise "Type mismatch" 
        end
        return by_int__(value)
    elsif type_name == "double"
        if !value.is_a?(Float64)
            raise "Type mismatch"
        end
        return by_double__(value)
    elsif type_name == "str"
        if !value.is_a?(String)
            raise "Type mismatch"
        end
        return by_string__(value)
    elsif type_name == "list"
        if !value.is_a?(Array)
            raise "Type mismatch"
        end
        return by_list__(value, ty)
    elsif type_name == "ulist"
        if !value.is_a?(Array)
            raise "Type mismatch"
        end
        return by_ulist__(value, ty)
    elsif type_name == "dict"
        if !value.is_a?(Hash)
            raise "Type mismatch"
        end
        return by_dict__(value, ty)
    elsif type_name == "option"
        return by_option__(value, ty)
    end
    raise "Unknown type #{type_name}"
end

def stringify__(value, type_str : String) : String
    return val_to_s__(value, s_to_type__(type_str)) + ":" + type_str
end

tfs = stringify__(true, "bool") + "\n" \
    + stringify__(3, "int") + "\n" \
    + stringify__(3.141592653, "double") + "\n" \
    + stringify__(3.0, "double") + "\n" \
    + stringify__("Hello, World!", "str") + "\n" \
    + stringify__("!@#$%^&*()\\\"\n\t", "str") + "\n" \
    + stringify__([1, 2, 3], "list<int>") + "\n" \
    + stringify__([true, false, true], "list<bool>") + "\n" \
    + stringify__([3, 2, 1], "ulist<int>") + "\n" \
    + stringify__({1 => "one", 2 => "two"}, "dict<int,str>") + "\n" \
    + stringify__({"one" => [1, 2, 3], "two" => [4, 5, 6]}, "dict<str,list<int>>") + "\n" \
    + stringify__(nil, "option<int>") + "\n" \
    + stringify__(3, "option<int>") + "\n"
File.write("stringify.out", tfs)