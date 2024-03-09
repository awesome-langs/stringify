<?php

function my_string_to_int($s) {
    return intval($s);
}

function my_string_to_double($s) {
    return floatval($s);
}

function my_int_to_string($i) {
    return strval($i);
}

function my_double_to_string($d) {
    return number_format($d, 6);
}

function my_bool_to_string($b) {
    return $b ? "true" : "false";
}

function my_int_to_nullable($i) {
    if ($i > 0) {
        return $i;
    } else if ($i < 0) {
        return -$i;
    } else {
        return null;
    }
}

function my_nullable_to_int($i) {
    return $i ?? -1;
}

function my_list_sorted($lst) {
    $tmp = $lst;
    sort($tmp);
    return $tmp;
}

function my_list_sorted_by_length($lst) {
    $tmp = $lst;
    usort($tmp, fn ($a, $b) => strlen($a) - strlen($b));
    return $tmp;
}

function my_list_filter($lst) {
    return array_filter($lst, fn ($x) => $x % 3 == 0);
}

function my_list_map($lst) {
    return array_map(fn ($x) => $x * $x, $lst);
}

function my_list_reduce($lst) {
    return array_reduce($lst, fn ($acc, $x) => $acc * 10 + $x, 0);
}

function my_list_operations($lst) {
    return array_reduce(
        array_map(fn ($x) => $x * $x, 
            array_filter($lst, fn ($x) => $x % 3 == 0)),
        fn ($acc, $x) => $acc * 10 + $x, 0);
}

function my_list_to_dict($lst) {
    return array_combine($lst, array_map(fn ($x) => $x * $x, $lst));
}

function my_dict_to_list($dict) {
    $tmp = $dict;
    ksort($tmp);
    return array_map(fn ($k, $v) => $k + $v, array_keys($tmp), $tmp);
}

function my_print_string($s) {
    echo $s . "\n";
}

function my_print_string_list($lst) {
    foreach ($lst as $x) {
        echo $x . " ";
    }
    echo "\n";
}

function my_print_int_list($lst) {
    my_print_string_list(array_map(fn ($x) => my_int_to_string($x), $lst));
}

function my_print_dict($dict) {
    foreach ($dict as $k => $v) {
        echo my_int_to_string($k) . "->" . my_int_to_string($v) . " ";
    }
    echo "\n";
}

my_print_string("Hello, World!");
my_print_string(my_int_to_string(my_string_to_int("123")));
my_print_string(my_double_to_string(my_string_to_double("123.456")));
my_print_string(my_bool_to_string(false));
my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(18))));
my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(-15))));
my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(0))));
my_print_string_list(my_list_sorted(["e", "dddd", "ccccc", "bb", "aaa"]));
my_print_string_list(my_list_sorted_by_length(["e", "dddd", "ccccc", "bb", "aaa"]));
my_print_string(my_int_to_string(my_list_reduce(my_list_map(my_list_filter([3, 12, 5, 8, 9, 15, 7, 17, 21, 11])))));
my_print_string(my_int_to_string(my_list_operations([3, 12, 5, 8, 9, 15, 7, 17, 21, 11])));
my_print_dict(my_list_to_dict([3, 1, 4, 2, 5, 9, 8, 6, 7, 0]));
my_print_int_list(my_dict_to_list([3 => 9, 1 => 1, 4 => 16, 2 => 4, 5 => 25, 9 => 81, 8 => 64, 6 => 36, 7 => 49, 0 => 0]));