-module(example).
-export([main/0]).
-record(poly_eval_type, {type_str, type_name, value_type, key_type}).

new_poly_eval_type__(TypeStr, TypeName, ValueType, KeyType) ->
    #poly_eval_type{type_str=TypeStr, type_name=TypeName, value_type=ValueType, key_type=KeyType}.

s_to_type__(TypeStr) ->
    case string:str(TypeStr, "<") of
        0 -> new_poly_eval_type__(TypeStr, TypeStr, undefined, undefined);
        Idx ->
            TypeName = string:substr(TypeStr, 1, Idx - 1),
            OtherStr = string:substr(TypeStr, Idx + 1, length(TypeStr) - Idx - 1),
            case string:str(OtherStr, ",") of
                0 ->
                    ValueType = s_to_type__(OtherStr),
                    new_poly_eval_type__(TypeStr, TypeName, ValueType, undefined);
                Idx2 ->
                    KeyType = s_to_type__(string:substr(OtherStr, 1, Idx2 - 1)),
                    ValueType = s_to_type__(string:substr(OtherStr, Idx2 + 1, length(OtherStr) - Idx2)),
                    new_poly_eval_type__(TypeStr, TypeName, ValueType, KeyType)
            end
    end.

escape_string__(S) ->
    NewS = lists:map(fun(C) ->
        case C of
            $\\ -> "\\\\";
            $\" -> "\\\"";
            $\n -> "\\n";
            $\t -> "\\t";
            $\r -> "\\r";
            _ -> [C]
        end
    end, S),
    string:join(NewS, "").

by_bool__(Value) ->
    case Value of
        true -> "true";
        false -> "false"
    end.

by_int__(Value) ->
    integer_to_list(Value).

by_double__(Value) ->
    Vs = lists:flatten(io_lib:format("~.6f", [Value])),
    Vs2 = re:replace(Vs, "0+$", "", [{return, list}]),
    Vs3 = re:replace(Vs2, "\\.$", ".0", [{return, list}]),
    case Vs3 of
        "-0.0" -> "0.0";
        _ -> Vs3
    end.

by_string__(Value) ->
    "\"" ++ escape_string__(Value) ++ "\"".

by_list__(Value, Ty) ->
    VStrs = lists:map(fun(V) -> val_to_s__(V, Ty#poly_eval_type.value_type) end, Value),
    "[" ++ string:join(VStrs, ", ") ++ "]".

by_ulist__(Value, Ty) ->
    VStrs = lists:map(fun(V) -> val_to_s__(V, Ty#poly_eval_type.value_type) end, Value),
    "[" ++ string:join(lists:sort(VStrs), ", ") ++ "]".

by_dict__(Value, Ty) ->
    VStrs = lists:map(fun({Key, Val}) -> val_to_s__(Key, Ty#poly_eval_type.key_type) ++ "=>" ++ val_to_s__(Val, Ty#poly_eval_type.value_type) end, maps:to_list(Value)),
    "{" ++ string:join(lists:sort(VStrs), ", ") ++ "}".

by_option__(Value, Ty) ->
    case Value of
        undefined -> "null";
        _ -> val_to_s__(Value, Ty#poly_eval_type.value_type)
    end.

val_to_s__(Value, Ty) ->
    TypeName = Ty#poly_eval_type.type_name,
    case TypeName of
        "bool" -> case Value of
            true -> by_bool__(Value);
            false -> by_bool__(Value);
            _ -> erlang:error("Type mismatch")
        end;
        "int" -> case Value of
            _ when is_integer(Value) -> by_int__(Value);
            _ -> erlang:error("Type mismatch")
        end;
        "double" -> case Value of
            _ when is_float(Value) -> by_double__(Value);
            _ -> erlang:error("Type mismatch")
        end;
        "str" -> case Value of
            _ when is_list(Value) -> by_string__(Value);
            _ -> erlang:error("Type mismatch")
        end;
        "list" -> case Value of
            _ when is_list(Value) -> by_list__(Value, Ty);
            _ -> erlang:error("Type mismatch")
        end;
        "ulist" -> case Value of
            _ when is_list(Value) -> by_ulist__(Value, Ty);
            _ -> erlang:error("Type mismatch")
        end;
        "dict" -> case Value of
            _ when is_map(Value) -> by_dict__(Value, Ty);
            _ -> erlang:error("Type mismatch")
        end;
        "option" -> by_option__(Value, Ty);
        _ -> erlang:error("Unknown type " ++ TypeName)
    end.

stringify__(Value, TypeStr) ->
    val_to_s__(Value, s_to_type__(TypeStr)) ++ ":" ++ TypeStr.

main() ->
    Tfs = stringify__(true, "bool") ++ "\n"
        ++ stringify__(3, "int") ++ "\n"
        ++ stringify__(3.141592653, "double") ++ "\n"
        ++ stringify__(3.0, "double") ++ "\n"
        ++ stringify__("Hello, World!", "str") ++ "\n"
        ++ stringify__("!@#$%^&*()\\\"\n\t", "str") ++ "\n"
        ++ stringify__([1, 2, 3], "list<int>") ++ "\n"
        ++ stringify__([true, false, true], "list<bool>") ++ "\n"
        ++ stringify__([3, 2, 1], "ulist<int>") ++ "\n"
        ++ stringify__(#{1 => "one", 2 => "two"}, "dict<int,str>") ++ "\n"
        ++ stringify__(#{"one" => [1, 2, 3], "two" => [4, 5, 6]}, "dict<str,list<int>>") ++ "\n"
        ++ stringify__(undefined, "option<int>") ++ "\n"
        ++ stringify__(3, "option<int>") ++ "\n",
    file:write_file("stringify.out", Tfs).