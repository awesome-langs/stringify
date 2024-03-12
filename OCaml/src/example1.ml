type poly_eval_type = {
    type_str: string;
    type_name: string;
    value_type: poly_eval_type option;
    key_type: poly_eval_type option;
}

let new_poly_eval_type__ type_str type_name value_type key_type =
    {type_str; type_name; value_type; key_type}

let rec s_to_type type_str =
    if String.contains type_str '<' then
        let idx = String.index type_str '<' in
        let type_name = String.sub type_str 0 idx in
        let other_str = String.sub type_str (idx + 1) (String.length type_str - idx - 2) in
        if String.contains other_str ',' then
            let idx = String.index other_str ',' in
            let key_type = s_to_type (String.sub other_str 0 idx) in
            let value_type = s_to_type (String.sub other_str (idx + 1) (String.length other_str - idx - 1)) in
            new_poly_eval_type__ type_str type_name (Some value_type) (Some key_type)
        else
            let value_type = s_to_type other_str in
            new_poly_eval_type__ type_str type_name (Some value_type) None
    else
        new_poly_eval_type__ type_str type_str None None

let escape_string s =
    let new_s = s |> String.to_seq |> List.of_seq |> List.map (fun c ->
        match c with
        | '\\' -> "\\\\"
        | '"' -> "\\\""
        | '\n' -> "\\n"
        | '\t' -> "\\t"
        | '\r' -> "\\r"
        | _ -> String.make 1 c
    ) in (String.concat "" new_s)

let by_bool value =
    if value then "true" else "false"

let by_int value =
    string_of_int value

let by_double value =
    let vs = ref (Printf.sprintf "%.6f" value) in
    while String.ends_with ~suffix:"0" !vs do
        vs := String.sub !vs 0 (String.length !vs - 1)
    done;
    if String.ends_with ~suffix:"." !vs then
        vs := !vs ^ "0";
    if !vs = "-0.0" then "0.0" else !vs

let by_string value =
    "\"" ^ escape_string value ^ "\""

let rec by_list value ty =
    let value_type = match ty.value_type with
        | Some value_type -> value_type
        | None -> raise (Invalid_argument "No ValueType") in
    let v_strs = List.map (fun v -> val_to_s v value_type ) value in
    "[" ^ (String.concat ", " v_strs) ^ "]"

and by_ulist value ty =
    let value_type = match ty.value_type with
        | Some value_type -> value_type
        | None -> raise (Invalid_argument "No ValueType") in
    let v_strs = List.map (fun v -> val_to_s v value_type) value in
    "[" ^ (String.concat ", " (List.sort compare v_strs)) ^ "]"

and by_dict value ty =
    let value_type = match ty.value_type with
        | Some value_type -> value_type
        | None -> raise (Invalid_argument "No ValueType") in
    let key_type = match ty.key_type with
        | Some key_type -> key_type
        | None -> raise (Invalid_argument "No KeyType") in 
    let v_strs = List.map (fun (key, value) -> val_to_s key key_type ^ "=>" ^ val_to_s value value_type) (List.of_seq (Hashtbl.to_seq value)) in
    "{" ^ (String.concat ", " (List.sort compare v_strs)) ^ "}"

and by_option value ty =
    match value with
    | None -> "null"
    | Some value -> let value_type = match ty.value_type with
        | Some value_type -> value_type
        | None -> raise (Invalid_argument "No ValueType") in
        val_to_s value value_type

and val_to_s value ty =
    match ty.type_name with
    | "bool" -> if value = true || value = false then by_bool value else raise (Invalid_argument "Type mismatch")
    | "int" -> if int_of_float value = value then by_int (int_of_float value) else raise (Invalid_argument "Type mismatch")
    | "double" -> if float_of_int value = value || float_of_int value = value then by_double value else raise (Invalid_argument "Type mismatch")
    | "str" -> if String value then by_string value else raise (Invalid_argument "Type mismatch")
    | "list" -> if List value then by_list value ty else raise (Invalid_argument "Type mismatch")
    | "ulist" -> if List value then by_ulist value ty else raise (Invalid_argument "Type mismatch")
    | "dict" -> if Hashtbl value then by_dict value ty else raise (Invalid_argument "Type mismatch")
    | "option" -> by_option value ty
    | _ -> raise (Invalid_argument ("Unknown type " ^ ty.type_name))

let stringify value type_str =
    val_to_s value (s_to_type type_str) ^ ":" ^ type_str

let () =
    let tfs = stringify true "bool" ^ "\n"
        ^ stringify 3 "int" ^ "\n"
        ^ stringify 3.141592653 "double" ^ "\n"
        ^ stringify 3.0 "double" ^ "\n"
        ^ stringify "Hello, World!" "str" ^ "\n"
        ^ stringify "!@#$%^&*()\\\"\n\t" "str" ^ "\n"
        ^ stringify [1; 2; 3] "list<int>" ^ "\n"
        ^ stringify [true; false; true] "list<bool>" ^ "\n"
        ^ stringify [3; 2; 1] "ulist<int>" ^ "\n"
        ^ stringify (Hashtbl.of_seq (List.to_seq [(1, "one"); (2, "two")])) "dict<int,str>" ^ "\n"
        ^ stringify (Hashtbl.of_seq (List.to_seq [("one", [1; 2; 3]); ("two", [4; 5; 6])])) "dict<str,list<int>>" ^ "\n"
        ^ stringify None "option<int>" ^ "\n"
        ^ stringify (Some 3) "option<int>" ^ "\n" in
    let oc = open_out "stringify.out" in
    Printf.fprintf oc "%s" tfs;
    close_out oc