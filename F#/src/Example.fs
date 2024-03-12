type PolyEvalType = {
    typeStr: string
    typeName: string
    valueType: PolyEvalType option
    keyType: PolyEvalType option
}

let newPolyEvalType (typeStr: string) (typeName: string) (valueType: PolyEvalType option) (keyType: PolyEvalType option): PolyEvalType =
    { typeStr = typeStr; typeName = typeName; valueType = valueType; keyType = keyType }

let rec sToType__ (typeStr: string): PolyEvalType =
    if not (typeStr.Contains "<") then
        newPolyEvalType typeStr typeStr None None
    else
        let idx = typeStr.IndexOf "<"
        let typeName = typeStr.Substring(0, idx)
        let otherStr = typeStr.Substring(idx + 1, typeStr.Length - idx - 2)
        if not (otherStr.Contains ",") then
            let valueType = sToType__ otherStr
            newPolyEvalType typeStr typeName (Some valueType) None
        else
            let idx = otherStr.IndexOf ","
            let keyType = sToType__ (otherStr.Substring(0, idx))
            let valueType = sToType__ (otherStr.Substring(idx + 1))
            newPolyEvalType typeStr typeName (Some valueType) (Some keyType)

let escapeString__ s =
    let newS = Seq.map (fun c ->
        match c with
        | '\\' -> "\\\\"
        | '\"' -> "\\\""
        | '\n' -> "\\n"
        | '\t' -> "\\t"
        | '\r' -> "\\r"
        | _ -> string c ) s
    String.concat "" newS
    
let byBool__ value: string =
    if value then "true" else "false"

let byInt__ value: string =
    value.ToString()

let byDouble__ value: string =
    let mutable vs = sprintf "%.6f" value
    while vs.EndsWith("0") do 
        vs <- vs.[..^1]
    vs <- if vs.EndsWith(".") then vs + "0" else vs
    if vs = "-0.0" then "0.0" else vs

let byString__ value: string =
    "\"" + escapeString__ value + "\""

let rec byList__ value (ty: PolyEvalType): string =
    let vStrs = List.map (fun v -> valToS__ v ty.valueType.Value) value
    "[" + (String.concat ", " vStrs) + "]"

and byUlist__ value (ty: PolyEvalType): string =
    let vStrs = List.map (fun v -> valToS__ v ty.valueType.Value) value
    "[" + (String.concat ", " (List.sort vStrs)) + "]"

and byDict__ value (ty: PolyEvalType): string =
    let vStrs = List.map (fun (k, v) -> (valToS__ k ty.keyType.Value) + "=>" + (valToS__ v ty.valueType.Value)) (value |> Seq.map (|KeyValue|) |> List.ofSeq)
    "{" + (String.concat ", " (List.sort vStrs)) + "}"

and byOption__ (value: obj) (ty: PolyEvalType): string =
    if value = null then "null"
    else if value.GetType().IsGenericType && value.GetType().GetGenericTypeDefinition() = typedefof<_ option> then
        valToS__ (value.GetType().GetProperty("Value").GetValue(value)) ty.valueType.Value
    else
        raise (System.Exception("Type mismatch"))

and valToS__ (value: obj) (ty: PolyEvalType): string =
    let typeName = ty.typeName
    match typeName with
    | "bool" -> 
        match value with 
            | :? bool as v -> byBool__ v
            | _ -> raise (System.Exception("Type mismatch"))
    | "int" -> 
        match value with 
            | :? int as v -> byInt__ v
            | _ -> raise (System.Exception("Type mismatch"))
    | "double" ->
        match value with 
            | :? double as v -> byDouble__ v
            | _ -> raise (System.Exception("Type mismatch"))
    | "str" ->
        match value with 
            | :? string as v -> byString__ v
            | _ -> raise (System.Exception("Type mismatch"))
    | "list" ->
        match value with 
            | :? System.Collections.IEnumerable as v -> 
                if value.GetType().IsGenericType && value.GetType().GetGenericTypeDefinition() = typedefof<_ list> then 
                    let mutable listValue: obj list = []
                    for v1 in v do
                        listValue <- listValue @ [v1]
                    byList__ listValue ty
                else raise (System.Exception("Type mismatch"))
            | _ -> raise (System.Exception("Type mismatch")) 
    | "ulist" ->
        match value with 
            | :? System.Collections.IEnumerable as v -> 
                if value.GetType().IsGenericType && value.GetType().GetGenericTypeDefinition() = typedefof<_ list> then
                    let mutable listValue: obj list = []
                    for v1 in v do
                        listValue <- listValue @ [v1]
                    byList__ listValue ty
                else raise (System.Exception("Type mismatch"))
            | _ -> raise (System.Exception("Type mismatch"))
    | "dict" ->
        match value with 
            | :? System.Collections.IEnumerable as v -> 
                if value.GetType().IsGenericType && value.GetType().GetGenericTypeDefinition() = typedefof<Map<_, _>> then
                    let dictValue: System.Collections.Generic.Dictionary<obj, obj> = new System.Collections.Generic.Dictionary<obj, obj>()
                    for kv in v do
                        dictValue.Add(kv.GetType().GetProperty("Key").GetValue(kv), kv.GetType().GetProperty("Value").GetValue(kv))
                    byDict__ dictValue ty
                else raise (System.Exception("Type mismatch"))
            | _ -> raise (System.Exception("Type mismatch"))
    | "option" -> byOption__ value ty
    | _ -> raise (System.Exception("Unknown type " + typeName))

let stringify__ (value: obj) (typeStr: string): string =
    valToS__ value (sToType__ typeStr) + ":" + typeStr

let tfs = (
    (stringify__ true "bool") + "\n"
    + (stringify__ 3 "int") + "\n"
    + (stringify__ 3.141592653 "double") + "\n"
    + (stringify__ 3.0 "double") + "\n"
    + (stringify__ "Hello, World!" "str") + "\n"
    + (stringify__ "!@#$%^&*()\\\"\n\t" "str") + "\n"
    + (stringify__ [1; 2; 3] "list<int>") + "\n"
    + (stringify__ [true; false; true] "list<bool>") + "\n"
    + (stringify__ [3; 2; 1] "ulist<int>") + "\n"
    + (stringify__ (Map [1, "one"; 2, "two"]) "dict<int,str>") + "\n"
    + (stringify__ (Map ["one", [1; 2; 3]; "two", [4; 5; 6]]) "dict<str,list<int>>") + "\n"
    + (stringify__ None "option<int>") + "\n"
    + (stringify__ (Some 3) "option<int>") + "\n"
)
System.IO.File.WriteAllText("stringify.out", tfs)