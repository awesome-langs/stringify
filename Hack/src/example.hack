use HH\Lib\C;
use HH\Lib\Vec;
use HH\Lib\Str;
use HH\Lib\Dict;

function my_string_to_int(string $s): int {
    return (int) $s;
}

function my_string_to_double(string $s): float {
    return (float) $s;
}

function my_int_to_string(int $i): string {
    return (string) $i;
}

function my_double_to_string(float $d): string {
    return sprintf("%.6f", $d);
}

function my_bool_to_string(bool $b): string {
    return $b ? "true" : "false";
}

function my_int_to_nullable(int $i): ?int {
    if ($i > 0) {
        return $i;
    } else if ($i < 0) {
        return -$i;
    } else {
        return null;
    }
}

function my_nullable_to_int(?int $i): int {
    return $i ?? -1;
}

function my_list_sorted(vec<string> $lst): vec<string> {
    return Vec\sort($lst);
}

function my_list_sorted_by_length(vec<string> $lst): vec<string> {
    return Vec\sort_by($lst, $x ==> Str\length($x));
}

function my_list_filter(vec<int> $lst): vec<int> {
    return Vec\filter($lst, $x ==> $x % 3 === 0);
}

function my_list_map(vec<int> $lst): vec<int> {
    return Vec\map($lst, $x ==> $x * $x);
}

function my_list_reduce(vec<int> $lst): int {
    return C\reduce($lst, ($acc, $x) ==> $acc * 10 + $x, 0);
}

function my_list_operations(vec<int> $lst): int {
    return $lst |> Vec\filter($$, $x ==> $x % 3 === 0)
        |> Vec\map($$, $x ==> $x * $x)
        |> C\reduce($$, ($acc, $x) ==> $acc * 10 + $x, 0);
}

function my_list_to_dict(vec<int> $lst): dict<int, int> {
    return Dict\from_keys($lst, $x ==> $x * $x);
}

function my_dict_to_list(dict<int, int> $dict): vec<int> {
    return $dict |> Dict\map_with_key($$, ($k, $v) ==> $k + $v)
        |> Dict\sort_by_key($$)
        |> vec($$);
}

function my_print_string(string $s): void {
    echo $s . "\n";
}

function my_print_string_list(vec<string> $lst): void {
    foreach ($lst as $x) {
        echo $x . " ";
    }
    echo "\n";
}

function my_print_int_list(vec<int> $lst): void {
    my_print_string_list(Vec\map($lst, $x ==> my_int_to_string($x)));
}

function my_print_dict(dict<int, int> $dict): void {
    foreach ($dict as $k => $v) {
        echo my_int_to_string($k) . "->" . my_int_to_string($v) . " ";
    }
    echo "\n";
}

<<__EntryPoint>>
function main(): void {
    my_print_string("Hello, World!");
    my_print_string(my_int_to_string(my_string_to_int("123")));
    my_print_string(my_double_to_string(my_string_to_double("123.456")));
    my_print_string(my_bool_to_string(false));
    my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(18))));
    my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(-15))));
    my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(0))));
    my_print_string_list(my_list_sorted(vec["e", "dddd", "ccccc", "bb", "aaa"]));
    my_print_string_list(my_list_sorted_by_length(vec["e", "dddd", "ccccc", "bb", "aaa"]));
    my_print_string(my_int_to_string(my_list_reduce(my_list_map(my_list_filter(vec[3, 12, 5, 8, 9, 15, 7, 17, 21, 11])))));
    my_print_string(my_int_to_string(my_list_operations(vec[3, 12, 5, 8, 9, 15, 7, 17, 21, 11])));
    my_print_dict(my_list_to_dict(vec[3, 1, 4, 2, 5, 9, 8, 6, 7, 0]));
    my_print_int_list(my_dict_to_list(dict[3 => 9, 1 => 1, 4 => 16, 2 => 4, 5 => 25, 9 => 81, 8 => 64, 6 => 36, 7 => 49, 0 => 0]));
}