def my_string_to_int(s : String) : Int32
    s.to_i
end

def my_string_to_double(s : String) : Float64
    s.to_f
end

def my_int_to_string(i : Int32) : String
    i.to_s
end

def my_double_to_string(d : Float64) : String
    "%.6f" % d
end

def my_bool_to_string(b : Bool) : String
    b ? "true" : "false"
end

def my_int_to_nullable(i : Int32) : Int32?
    if i > 0
        i
    elsif i < 0
        -i
    else
        nil
    end
end

def my_nullable_to_int(i : Int32?) : Int32
    i || -1
end

def my_list_sorted(lst : Array(String)) : Array(String)
    lst.sort
end

def my_list_sorted_by_length(lst : Array(String)) : Array(String)
    lst.sort_by &.size
end

def my_list_filter(lst : Array(Int32)) : Array(Int32)
    lst.select { |x| x.modulo(3) == 0 }
end

def my_list_map(lst : Array(Int32)) : Array(Int32)
    lst.map { |x| x * x }
end

def my_list_reduce(lst : Array(Int32)) : Int32
    lst.reduce(0) { |acc, x| acc * 10 + x }
end

def my_list_operations(lst : Array(Int32)) : Int32
    lst.select { |x| x.modulo(3) == 0 }
        .map { |x| x * x }
        .reduce(0) { |acc, x| acc * 10 + x }
end

def my_list_to_dict(lst : Array(Int32)) : Hash(Int32, Int32)
    lst.map { |x| [x, x * x] }.to_h
end

def my_dict_to_list(dict : Hash(Int32, Int32)) : Array(Int32)
    dict.to_a.sort_by(&.first).map { |k, v| k + v }
end

def my_print_string(s : String)
    puts s
end

def my_print_string_list(lst : Array(String))
    lst.each { |x| 
        print x + " " 
    }
    puts
end

def my_print_int_list(lst : Array(Int32))
    my_print_string_list(lst.map &.to_s)
end

def my_print_dict(dict : Hash(Int32, Int32))
    dict.each { |k, v| 
        print my_int_to_string(k) + "->" + my_int_to_string(v) + " "
    }
    puts
end

my_print_string("Hello, World!")
my_print_string(my_int_to_string(my_string_to_int "123"))
my_print_string(my_double_to_string(my_string_to_double("123.456")))
my_print_string(my_bool_to_string(false))
my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(18))))
my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(-15))))
my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(0))))
my_print_string_list(my_list_sorted(["e", "dddd", "ccccc", "bb", "aaa"]))
my_print_string_list(my_list_sorted_by_length(["e", "dddd", "ccccc", "bb", "aaa"]))
my_print_string(my_int_to_string(my_list_reduce(my_list_map(my_list_filter([3, 12, 5, 8, 9, 15, 7, 17, 21, 11])))))
my_print_string(my_int_to_string(my_list_operations([3, 12, 5, 8, 9, 15, 7, 17, 21, 11])))
my_print_dict(my_list_to_dict([3, 1, 4, 2, 5, 9, 8, 6, 7, 0]))
my_print_int_list(my_dict_to_list({3 => 9, 1 => 1, 4 => 16, 2 => 4, 5 => 25, 9 => 81, 8 => 64, 6 => 36, 7 => 49, 0 => 0}))