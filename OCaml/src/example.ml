module type Showable = sig
    type t
    val to_string : t -> string
end

module BoolShowable : Showable with type t = bool = struct
    type t = bool
    let to_string = function
        | true -> "true"
        | false -> "false"
end

module IntShowable : Showable with type t = int = struct
    type t = int
    let to_string = string_of_int
end

let myshow (type a) (module S : Showable with type t = a) (x : a) =
    S.to_string x

let myshow_1 (type a) (s : string) (x : a) =
    if s = "bool" then
        myshow (module BoolShowable) x
    else if s = "int" then
        myshow (module IntShowable) x
    else
        "unknown"


let () =
    print_endline (myshow_1 "bool" true);
    print_endline (myshow_1 "int" 42)
