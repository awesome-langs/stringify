function my_string_to_int(s)
    return tonumber(s)
end

function my_string_to_double(s)
    return tonumber(s)
end

function my_int_to_string(i)
    return tostring(i)
end

function my_double_to_string(d)
    return string.format("%.6f", d)
end

function my_bool_to_string(b)
    return b and "true" or "false"
end

function my_int_to_nullable(i)
    if i > 0 then
      return i
    elseif i < 0 then
      return -i
    else
      return nil
    end
end

function my_nullable_to_int(i)
    return i or -1
end

function my_list_sorted(lst)
    local tmp = {}
    for i, v in ipairs(lst) do
      tmp[i] = v
    end
    table.sort(tmp)
    return tmp
end

function my_list_sorted_by_length(lst)
    local tmp = {}
    for i, v in ipairs(lst) do
      tmp[i] = v
    end
    table.sort(tmp, function(a, b) return #a < #b end)
    return tmp
end

function my_list_filter(lst)
    local tmp = {}
    for i, v in ipairs(lst) do
        if v % 3 == 0 then
            table.insert(tmp, v)
        end
    end
    return tmp
end


function my_list_map(lst)
    local tmp = {}
    for i, v in ipairs(lst) do
        tmp[i] = v * v
    end
    return tmp
    -- 
end

function my_list_reduce(lst)
    local tmp = 0
    for i, v in ipairs(lst) do
        tmp = tmp * 10 + v
    end
    return tmp
end

function my_list_operations(lst)
    local tmp = 0
    for i, v in ipairs(lst) do
        if v % 3 == 0 then
            tmp = tmp * 10 + v * v
        end
    end
    return tmp
end

function my_list_to_dict(lst)
    local tmp = {}
    for i, v in ipairs(lst) do
        tmp[v] = v * v
    end
    return tmp
end

function my_dict_to_list(dict)
    local keys = {}
    for k, v in pairs(dict) do
        table.insert(keys, k)
    end
    table.sort(keys)
    local tmp = {}
    for i, k in ipairs(keys) do
        table.insert(tmp, k + dict[k])
    end
    return tmp
end

function my_print_string(s)
    print(s)
end

function my_print_string_list(lst)
    for i, v in ipairs(lst) do
        io.write(v .. " ")
    end
    print()
end

function my_print_int_list(lst)
    local tmp = {}
    for i, v in ipairs(lst) do
        table.insert(tmp, my_int_to_string(v))
    end
    my_print_string_list(tmp)
end

function my_print_dict(dict)
    for k, v in pairs(dict) do
        io.write(my_int_to_string(k) .. "->" .. my_int_to_string(v) .. " ")
    end
    print()
end

my_print_string("Hello, World!")
my_print_string(my_int_to_string(my_string_to_int("123")))
my_print_string(my_double_to_string(my_string_to_double("123.456")))
my_print_string(my_bool_to_string(false))
my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(18))))
my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(-15))))
my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(0))))
my_print_string_list(my_list_sorted({"e", "dddd", "ccccc", "bb", "aaa"}))
my_print_string_list(my_list_sorted_by_length({"e", "dddd", "ccccc", "bb", "aaa"}))
my_print_string(my_int_to_string(my_list_reduce(my_list_map(my_list_filter({3, 12, 5, 8, 9, 15, 7, 17, 21, 11})))))
my_print_string(my_int_to_string(my_list_operations({3, 12, 5, 8, 9, 15, 7, 17, 21, 11})))
my_print_dict(my_list_to_dict({3, 1, 4, 2, 5, 9, 8, 6, 7, 0}))
my_print_int_list(my_dict_to_list({[3] = 9, [1] = 1, [4] = 16, [2] = 4, [5] = 25, [9] = 81, [8] = 64, [6] = 36, [7] = 49, [0] = 0}))