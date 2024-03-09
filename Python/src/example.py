
from functools import reduce

def my_string_to_int(s):
    return int(s)

def my_string_to_double(s):
    return float(s)

def my_int_to_string(i):
    return str(i)

def my_double_to_string(d):
    return "{:.6f}".format(d)

def my_bool_to_string(b):
    return "true" if b else "false"

def my_int_to_nullable(i):
    if i > 0:
        return i
    elif i < 0:
        return -i
    else:
        return None

def my_nullable_to_int(i):
    return i if i is not None else -1

def my_list_sorted(lst):
    return sorted(lst)

def my_list_sorted_by_length(lst):
    return sorted(lst, key=len)

def my_list_filter(lst):
    return list(filter(lambda x: x % 3 == 0, lst))

def my_list_map(lst):
    return list(map(lambda x: x * x, lst))

def my_list_reduce(lst):
    return reduce(lambda acc, x: acc * 10 + x, lst, 0)

def my_list_operations(lst):
    return reduce(lambda acc, x: acc * 10 + x, 
        map(lambda x: x * x, 
            filter(lambda x: x % 3 == 0, lst)), 0)

def my_list_to_dict(lst):
    return dict(map(lambda x: (x, x * x), lst))

def my_dict_to_list(d):
    return list(map(lambda x: x[0] + x[1], sorted(d.items(), key=lambda x: x[0])))

def my_print_string(s):
    print(s)

def my_print_string_list(lst):
    for x in lst:
        print(x + " ", end="")
    print()

def my_print_int_list(lst):
    my_print_string_list(map(my_int_to_string, lst))

def my_print_dict(d):
    for k, v in d.items():
        print(my_int_to_string(k) + "->" + my_int_to_string(v) + " ", end="")
    print()

my_print_string("Hello, World!")
my_print_string(my_int_to_string(my_string_to_int("123")))
my_print_string(my_double_to_string(my_string_to_double("123.456")))
my_print_string(my_bool_to_string(False))
my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(18))))
my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(-15))))
my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(0))))
my_print_string_list(my_list_sorted(["e", "dddd", "ccccc", "bb", "aaa"]))
my_print_string_list(my_list_sorted_by_length(["e", "dddd", "ccccc", "bb", "aaa"]))
my_print_string(my_int_to_string(my_list_reduce(my_list_map(my_list_filter([3, 12, 5, 8, 9, 15, 7, 17, 21, 11])))))
my_print_string(my_int_to_string(my_list_operations([3, 12, 5, 8, 9, 15, 7, 17, 21, 11])))
my_print_dict(my_list_to_dict([3, 1, 4, 2, 5, 9, 8, 6, 7, 0]))
my_print_int_list(my_dict_to_list({3: 9, 1: 1, 4: 16, 2: 4, 5: 25, 9: 81, 8: 64, 6: 36, 7: 49, 0: 0}))