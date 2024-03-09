-module(example).
-export([main/0]).

my_string_to_int(S) -> 
    list_to_integer(S).

my_string_to_double(S) ->
    list_to_float(S).

my_int_to_string(I) ->
    integer_to_list(I).

my_double_to_string(D) ->
    lists:flatten(io_lib:format("~.6f", [D])).

my_bool_to_string(B) ->
    if B -> "true"; true -> "false" end.

my_int_to_nullable(I) ->
    if
        I > 0 -> I;
        I < 0 -> -I;
        true -> undefined
    end.

my_nullable_to_int(I) ->
    case I of
        undefined -> -1;
        X -> X
    end.

my_list_sorted(Lst) ->
    lists:sort(Lst).

my_list_sorted_by_length(Lst) ->
    lists:sort(fun(A, B) -> length(A) < length(B) end, Lst).

my_list_filter(Lst) ->
    lists:filter(fun(X) -> X rem 3 =:= 0 end, Lst).

my_list_map(Lst) ->
    lists:map(fun(X) -> X * X end, Lst).

my_list_reduce(Lst) ->
    lists:foldl(fun(X, Acc) -> Acc * 10 + X end, 0, Lst).

my_list_operations(Lst) ->
    lists:foldl(fun(X, Acc) -> Acc * 10 + X end, 0, 
        lists:map(fun(X) -> X * X end, 
            lists:filter(fun(X) -> X rem 3 =:= 0 end, Lst))).

my_list_to_dict(Lst) ->
    dict:from_list(lists:map(fun(X) -> {X, X * X} end, Lst)).

my_dict_to_list(Dict) ->
    lists:map(fun({K, V}) -> K + V end, lists:sort(fun({A, _}, {B, _}) -> A < B end, dict:to_list(Dict))).

my_print_string(S) ->
    io:format("~s~n", [S]).

my_print_string_list(Lst) ->
    lists:foreach(fun(X) -> 
        io:format("~s ", [X]) 
    end, Lst),
    io:format("~n").

my_print_int_list(Lst) ->
    my_print_string_list(lists:map(fun(X) -> my_int_to_string(X) end, Lst)).

my_print_dict(Dict) ->
    lists:foreach(fun({K, V}) -> 
        io:format("~s->~s ", [my_int_to_string(K), my_int_to_string(V)]) 
    end, dict:to_list(Dict)),
    io:format("~n").

main() ->
    my_print_string("Hello, World!"),
    my_print_string(my_int_to_string(my_string_to_int("123"))),
    my_print_string(my_double_to_string(my_string_to_double("123.456"))),
    my_print_string(my_bool_to_string(false)),
    my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(18)))),
    my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(-15)))),
    my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(0)))),
    my_print_string_list(my_list_sorted(["e", "dddd", "ccccc", "bb", "aaa"])),
    my_print_string_list(my_list_sorted_by_length(["e", "dddd", "ccccc", "bb", "aaa"])),
    my_print_string(my_int_to_string(my_list_reduce(my_list_map(my_list_filter([3, 12, 5, 8, 9, 15, 7, 17, 21, 11]))))),
    my_print_string(my_int_to_string(my_list_operations([3, 12, 5, 8, 9, 15, 7, 17, 21, 11]))),
    my_print_dict(my_list_to_dict([3, 1, 4, 2, 5, 9, 8, 6, 7, 0])),
    my_print_int_list(my_dict_to_list(dict:from_list([{3, 9}, {1, 1}, {4, 16}, {2, 4}, {5, 25}, {9, 81}, {8, 64}, {6, 36}, {7, 49}, {0, 0}]))).