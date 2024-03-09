def my_string_to_int(s)
    s.to_i
end

def my_string_to_double(s)
    s.to_f
end

def my_int_to_string(i)
    i.to_s
end

def my_double_to_string(d)
    format("%.6f", d)
end

def my_bool_to_string(b)
    b ? "true" : "false"
end

def my_int_to_nullable(i)
    if i > 0
        i
    elsif i < 0
        -i
    else
        nil
    end
end

def my_nullable_to_int(i)
    i.nil? ? -1 : i
end

def my_list_sorted(lst)
    lst.sort
end

def my_list_sorted_by_length(lst)
    lst.sort_by(&:length)
end

def my_list_filter(lst)
    lst.select { |x| x % 3 == 0 }
end

def my_list_map(lst)
    lst.map { |x| x * x }
end

def my_list_reduce(lst)
    lst.reduce(0) { |acc, x| acc * 10 + x }
end

def my_list_operations(lst)
    lst.select { |x| x % 3 == 0 }
        .map { |x| x * x }
        .reduce(0) { |acc, x| acc * 10 + x }
end

def my_list_to_dict(lst)
    lst.map { |x| [x, x * x] }.to_h
end

def my_dict_to_list(dict)
    dict.sort_by(&:first).map { |k, v| k + v }
end

def my_print_string(s)
    puts s
end

def my_print_string_list(lst)
    for x in lst
        print x + " "
    end
    puts
end

def my_print_int_list(lst)
    my_print_string_list(lst.map { |x| my_int_to_string(x) })
end

def my_print_dict(dict)
    for k, v in dict
        print my_int_to_string(k) + "->" + my_int_to_string(v) + " "
    end
    puts
end

my_print_string("Hello, World!")
my_print_string(my_int_to_string(my_string_to_int("123")))
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
