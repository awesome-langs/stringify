class PolyEvalType
    attr_accessor :type_str, :type_name, :value_type, :key_type
    
    def initialize(type_str, type_name, value_type, key_type)
        @type_str = type_str
        @type_name = type_name
        @value_type = value_type
        @key_type = key_type
    end
end

def s_to_type__(type_str)
    if not type_str.include? "<"
        return PolyEvalType.new(type_str, type_str, nil, nil)
    else
        idx = type_str.index "<"
        type_name = type_str[0...idx]
        other_str = type_str[idx + 1...-1]
        if not other_str.include? ","
            value_type = s_to_type__(other_str)
            return PolyEvalType.new(type_str, type_name, value_type, nil)
        else
            idx = other_str.index ","
            key_type = s_to_type__(other_str[0...idx])
            value_type = s_to_type__(other_str[idx + 1..-1])
            return PolyEvalType.new(type_str, type_name, value_type, key_type)
        end
    end
end

def escape_string__(s)
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

def by_bool__(value)
    return value ? "true" : "false"
end

def by_int__(value)
    v = value.to_i
    return v.to_s
end

def by_double__(value)
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

def by_string__(value)
    return '"' + escape_string__(value) + '"'
end

def by_list__(value, ty)
    v_strs = value.map do |v|
        val_to_s__(v, ty.value_type)
    end
    return "[" + v_strs.join(", ") + "]"
end

def by_ulist__(value, ty)
    v_strs = value.map do |v|
        val_to_s__(v, ty.value_type)
    end
    return "[" + v_strs.sort.join(", ") + "]"
end

def by_dict__(value, ty)
    v_strs = value.map do |key, val|
        val_to_s__(key, ty.key_type) + "=>" + val_to_s__(val, ty.value_type)
    end
    return "{" + v_strs.sort.join(", ") + "}"
end

def by_option__(value, ty)
    if value.nil?
        return "null"
    else
        return val_to_s__(value, ty.value_type)
    end
end

def val_to_s__(value, ty)
    type_name = ty.type_name
    if type_name == "bool"
        if not value.is_a? TrueClass and not value.is_a? FalseClass
            raise "Type mismatch"
        end
        return by_bool__(value)
    elsif type_name == "int"
        if not value.is_a? Integer and not (value.is_a? Float and value.to_i == value)
            raise "Type mismatch"
        end
        return by_int__(value)
    elsif type_name == "double"
        if not value.is_a? Integer and not value.is_a? Float
            raise "Type mismatch"
        end
        return by_double__(value)
    elsif type_name == "str"
        if not value.is_a? String
            raise "Type mismatch"
        end
        return by_string__(value)
    elsif type_name == "list"
        if not value.is_a? Array
            raise "Type mismatch"
        end
        return by_list__(value, ty)
    elsif type_name == "ulist"
        if not value.is_a? Array
            raise "Type mismatch"
        end
        return by_ulist__(value, ty)
    elsif type_name == "dict"
        if not value.is_a? Hash
            raise "Type mismatch"
        end
        return by_dict__(value, ty)
    elsif type_name == "option"
        return by_option__(value, ty)
    end
    raise "Unknown type #{type_name}"
end

def stringify__(value, type_str)
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