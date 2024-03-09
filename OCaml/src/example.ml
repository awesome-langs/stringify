let my_string_to_int (s : string) : int =
    int_of_string s

let my_string_to_double (s : string) : float =
    float_of_string s

let my_int_to_string (i : int) : string =
    string_of_int i

let my_double_to_string (d : float) : string =
    Printf.sprintf "%.6f" d

let my_bool_to_string (b : bool) : string =
    if b then "true" else "false"

let my_int_to_nullable (i : int) : int option =
    if i > 0 then Some i
    else if i < 0 then Some (-i)
    else None

let my_nullable_to_int (i : int option) : int =
    match i with
    | Some x -> x
    | None -> -1

let my_list_sorted (lst : string list) : string list =
    List.sort compare lst

let my_list_sorted_by_length (lst : string list) : string list =
    List.sort (fun x y -> compare (String.length x) (String.length y)) lst

let my_list_filter (lst : int list) : int list =
    List.filter (fun x -> x mod 3 = 0) lst

let my_list_map (lst : int list) : int list =
    List.map (fun x -> x * x) lst

let my_list_reduce (lst : int list) : int =
    List.fold_left (fun acc x -> acc * 10 + x) 0 lst

let my_list_operations (lst : int list) : int =
    lst |> List.filter (fun x -> x mod 3 = 0)
        |> List.map (fun x -> x * x)
        |> List.fold_left (fun acc x -> acc * 10 + x) 0

let my_list_to_dict (lst : int list) : (int, int) Hashtbl.t =
    Hashtbl.of_seq (List.to_seq (List.map (fun x -> (x, x * x)) lst))

let my_dict_to_list (dict : (int, int) Hashtbl.t) : int list =
    dict |> Hashtbl.to_seq |> List.of_seq
        |> List.sort (fun (k1, _) (k2, _) -> compare k1 k2)
        |> List.map (fun (k, v) -> k + v)

let my_print_string (s : string) : unit =
    print_endline s

let my_print_string_list (lst : string list) : unit =
    List.iter (fun x -> print_string (x ^ " ")) lst;
    print_endline ""

let my_print_int_list (lst : int list) : unit =
    lst |> List.map my_int_to_string |> my_print_string_list

let my_print_dict (dict : (int, int) Hashtbl.t) : unit =
    Hashtbl.iter (fun k v -> print_string (my_int_to_string k ^ "->" ^ my_int_to_string v ^ " ")) dict;
    print_endline ""

let () = 
    my_print_string "Hello, World!";
    my_print_string (my_int_to_string (my_string_to_int "123"));
    my_print_string (my_double_to_string (my_string_to_double "123.456"));
    my_print_string (my_bool_to_string false);
    my_print_string (my_int_to_string (my_nullable_to_int (my_int_to_nullable 18)));
    my_print_string (my_int_to_string (my_nullable_to_int (my_int_to_nullable (-15))));
    my_print_string (my_int_to_string (my_nullable_to_int (my_int_to_nullable 0)));
    my_print_string_list (my_list_sorted ["e"; "dddd"; "ccccc"; "bb"; "aaa"]);
    my_print_string_list (my_list_sorted_by_length ["e"; "dddd"; "ccccc"; "bb"; "aaa"]);
    my_print_string (my_int_to_string (my_list_reduce (my_list_map (my_list_filter [3; 12; 5; 8; 9; 15; 7; 17; 21; 11]))));
    my_print_string (my_int_to_string (my_list_operations [3; 12; 5; 8; 9; 15; 7; 17; 21; 11]));
    my_print_dict (my_list_to_dict [3; 1; 4; 2; 5; 9; 8; 6; 7; 0]);
    my_print_int_list (my_dict_to_list (Hashtbl.of_seq (List.to_seq[(3, 9); (1, 1); (4, 16); (2, 4); (5, 25); (9, 81); (8, 64); (6, 36); (7, 49); (0, 0)])))