use HH\Lib\Vec;
use HH\Lib\Str;
use HH\Lib\Dict;

class PolyEvalType {
    public string $type_str;
    public string $type_name;
    public ?PolyEvalType $value_type;
    public ?PolyEvalType $key_type;

    public function __construct(string $type_str, string $type_name, ?PolyEvalType $value_type, ?PolyEvalType $key_type) {
        $this->type_str = $type_str;
        $this->type_name = $type_name;
        $this->value_type = $value_type;
        $this->key_type = $key_type;
    }
}

function s_to_type__(string $type_str): PolyEvalType {
    if (strpos($type_str, "<") === false) {
        return new PolyEvalType($type_str, $type_str, null, null);
    } else {
        $idx = strpos($type_str, "<");
        $type_name = substr($type_str, 0, $idx);
        $other_str = substr($type_str, $idx + 1, -1);
        if (strpos($other_str, ",") === false) {
            $value_type = s_to_type__($other_str);
            return new PolyEvalType($type_str, $type_name, $value_type, null);
        } else {
            $idx = strpos($other_str, ",");
            $key_type = s_to_type__(substr($other_str, 0, $idx));
            $value_type = s_to_type__(substr($other_str, $idx + 1));
            return new PolyEvalType($type_str, $type_name, $value_type, $key_type);
        }
    }
}

function escape_string__(string $s): string {
    $new_s = vec[];
    for ($i = 0; $i < strlen($s); $i++) {
        $c = $s[$i];
        if ($c === "\\") {
            $new_s[] = "\\\\";
        } else if ($c === "\"") {
            $new_s[] = "\\\"";
        } else if ($c === "\n") {
            $new_s[] = "\\n";
        } else if ($c === "\t") {
            $new_s[] = "\\t";
        } else if ($c === "\r") {
            $new_s[] = "\\r";
        } else {
            $new_s[] = $c;
        }
    }
    return implode("", $new_s);
}

function by_bool__(bool $value): string {
    return $value ? "true" : "false";
}

function by_int__(int $value): string {
    return (string)$value;
}

function by_double__(float $value): string {
    $vs = sprintf("%.6f", $value);
    while (substr($vs, -1) === "0") {
        $vs = substr($vs, 0, -1);
    }
    if (substr($vs, -1) === ".") {
        $vs .= "0";
    }
    if ($vs === "-0.0") {
        $vs = "0.0";
    }
    return $vs;
}

function by_string__(string $value): string {
    return '"' . escape_string__($value) . '"';
}

function by_list__(vec<mixed> $value, PolyEvalType $ty): string {
    $v_strs = Vec\map($value, $v ==> val_to_s__($v, $ty->value_type));
    return "[" . implode(", ", $v_strs) . "]";
}

function by_ulist__(vec<mixed> $value, PolyEvalType $ty): string {
    $v_strs = Vec\map($value, $v ==> val_to_s__($v, $ty->value_type));
    return "[" . implode(", ", Vec\sort($v_strs)) . "]";
}

function by_dict__(dict<mixed, mixed> $value, PolyEvalType $ty): string {
    $v_strs = Dict\map_with_key($value, ($key, $val) ==> val_to_s__($key, $ty->key_type) . "=>" . val_to_s__($val, $ty->value_type));
    return "{" . implode(", ", Vec\sort($v_strs)) . "}";
}

function by_option__(mixed $value, PolyEvalType $ty): string {
    if ($value === null) {
        return "null";
    } else {
        return val_to_s__($value, $ty->value_type);
    }
}

function val_to_s__(mixed $value, PolyEvalType $ty): string {
    $type_name = $ty->type_name;
    if ($type_name === "bool") {
        if (!is_bool($value)) {
            throw new \Exception("Type mismatch");
        }
        return by_bool__($value);
    } else if ($type_name === "int") {
        if (!is_int($value) && !(is_float($value) && (int)$value === $value)) {
            throw new \Exception("Type mismatch");
        }
        return by_int__($value);
    } else if ($type_name === "double") {
        if (!is_int($value) && !is_float($value)) {
            throw new \Exception("Type mismatch");
        }
        return by_double__($value);
    } else if ($type_name === "str") {
        if (!is_string($value)) {
            throw new \Exception("Type mismatch");
        }
        return by_string__($value);
    } else if ($type_name === "list") {
        if (!is_array($value)) {
            throw new \Exception("Type mismatch");
        }
        return by_list__($value, $ty);
    } else if ($type_name === "ulist") {
        if (!is_array($value)) {
            throw new \Exception("Type mismatch");
        }
        return by_ulist__($value, $ty);
    } else if ($type_name === "dict") {
        if (!is_array($value)) {
            throw new \Exception("Type mismatch");
        }
        return by_dict__($value, $ty);
    } else if ($type_name === "option") {
        return by_option__($value, $ty);
    }
    throw new \Exception("Unknown type " . $type_name);
}

function stringify__(mixed $value, string $type_str): string {
    return val_to_s__($value, s_to_type__($type_str)) . ":" . $type_str;
}

<<__EntryPoint>>
function main(): void {
    $tfs = stringify__(true, "bool") . "\n" .
        stringify__(3, "int") . "\n" .
        stringify__(3.141592653, "double") . "\n" .
        stringify__(3.0, "double") . "\n" .
        stringify__("Hello, World!", "str") . "\n" .
        stringify__("!@#$%^&*()\\\"\n\t", "str") . "\n" .
        stringify__(vec[1, 2, 3], "list<int>") . "\n" .
        stringify__(vec[true, false, true], "list<bool>") . "\n" .
        stringify__(vec[3, 2, 1], "ulist<int>") . "\n" .
        stringify__(dict[1 => "one", 2 => "two"], "dict<int,str>") . "\n" .
        stringify__(dict["one" => vec[1, 2, 3], "two" => vec[4, 5, 6]], "dict<str,list<int>>") . "\n" .
        stringify__(null, "option<int>") . "\n" .
        stringify__(3, "option<int>") . "\n";
    file_put_contents("stringify.out", $tfs);
}