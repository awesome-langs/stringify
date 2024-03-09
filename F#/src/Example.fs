let myStringToInt (s: string): int = 
    int s

let myStringToDouble (s: string): double =
    double s

let myIntToString (i: int): string =
    i.ToString()

let myDoubleToString (d: double): string =
    sprintf "%.6f" d

let myBoolToString (b: bool): string =
    if b then "true" else "false"

let myIntToNullable (i: int): int option =
    if i > 0 then Some i
    elif i < 0 then Some -i
    else None

let myNullableToInt (i: int option): int =
    match i with
    | Some x -> x
    | None -> -1

let myListSorted (lst: string list): string list =
    List.sort lst

let myListSortedByLength (lst: string list): string list =
    List.sortBy (fun x -> x.Length) lst

let myListFilter (lst: int list): int list =
    List.filter (fun x -> x % 3 = 0) lst

let myListMap (lst: int list): int list =
    List.map (fun x -> x * x) lst

let myListReduce (lst: int list): int =
    List.fold (fun acc x -> acc * 10 + x) 0 lst

let myListOperations (lst: int list): int =
    lst |> List.filter (fun x -> x % 3 = 0)
        |> List.map (fun x -> x * x)
        |> List.fold (fun acc x -> acc * 10 + x) 0

let myListToDict (lst: int list): Map<int, int> =
    lst |> List.map (fun x -> x, x * x) |> Map.ofList

let myDictToList (dict: Map<int, int>): int list =
    dict |> Map.toList |> List.sortBy (fun (k, v) -> k) |> List.map (fun (k, v) -> k + v)

let myPrintString (s: string): unit =
    printfn "%s" s

let myPrintStringList (lst: string list): unit =
    for x in lst do
        printf "%s " x
    printfn ""

let myPrintIntList (lst: int list): unit =
    lst |> List.map (fun x -> myIntToString x) |> myPrintStringList

let myPrintDict (dict: Map<int, int>): unit =
    for KeyValue(k, v) in dict do
        printf "%s->%s " (myIntToString k) (myIntToString v)
    printfn ""

myPrintString "Hello, World!"
myPrintString (myIntToString (myStringToInt "123"))
myPrintString (myDoubleToString (myStringToDouble "123.456"))
myPrintString (myBoolToString false)
myPrintString (myIntToString (myNullableToInt (myIntToNullable 18)))
myPrintString (myIntToString (myNullableToInt (myIntToNullable -15)))
myPrintString (myIntToString (myNullableToInt (myIntToNullable 0)))
myPrintStringList (myListSorted ["e"; "dddd"; "ccccc"; "bb"; "aaa"])
myPrintStringList (myListSortedByLength ["e"; "dddd"; "ccccc"; "bb"; "aaa"])
myPrintString (myIntToString (myListReduce (myListMap (myListFilter [3; 12; 5; 8; 9; 15; 7; 17; 21; 11]))))
myPrintString (myIntToString (myListOperations [3; 12; 5; 8; 9; 15; 7; 17; 21; 11]))
myPrintDict (myListToDict [3; 1; 4; 2; 5; 9; 8; 6; 7; 0])
myPrintIntList (myDictToList (Map [3, 9; 1, 1; 4, 16; 2, 4; 5, 25; 9, 81; 8, 64; 6, 36; 7, 49; 0, 0]))

let myPrintListofOptionDict (dict: Map<int, int> option list): unit =
    for x in dict do
        match x with
        | Some d -> myPrintDict d
        | None -> printfn "None"

myPrintListofOptionDict [Some (myListToDict [3; 1; 4; 2; 5; 9; 8; 6; 7; 0]); None; Some (myListToDict [3; 1; 4; 2; 5; 9; 8; 6; 7; 0])]