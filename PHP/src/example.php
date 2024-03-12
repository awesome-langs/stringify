<?php

class PolyEvalType {
    public $type_str;
    public $type_name;
    public $value_type;
    public $key_type;

    public function __construct($type_str, $type_name, $value_type, $key_type) {
        $this->type_str = $type_str;
        $this->type_name = $type_name;
        $this->value_type = $value_type;
        $this->key_type = $key_type;
    }
}

function s_to_type__($type_str) {
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

function escape_string__($s) {
    $new_s = [];
    foreach (str_split($s) as $c) {
        if ($c === "\\") {
            $new_s[] = "\\\\";
        } elseif ($c === "\"") {
            $new_s[] = "\\\"";
        } elseif ($c === "\n") {
            $new_s[] = "\\n";
        } elseif ($c === "\t") {
            $new_s[] = "\\t";
        } elseif ($c === "\r") {
            $new_s[] = "\\r";
        } else {
            $new_s[] = $c;
        }
    }
    return implode("", $new_s);
}

function by_bool__($value) {
    return $value ? "true" : "false";
}

function by_int__($value) {
    return strval($value);
}

function by_double__($value) {
    $v = floatval($value);
    $vs = number_format($v, 6, ".", "");
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

function by_string__($value) {
    return '"' . escape_string__($value) . '"';
}

function by_list__($value, $ty) {
    $v_strs = array_map(function($v) use ($ty) {
        return val_to_s__($v, $ty->value_type);
    }, $value);
    return "[" . implode(", ", $v_strs) . "]";
}

function by_ulist__($value, $ty) {
    $v_strs = array_map(function($v) use ($ty) {
        return val_to_s__($v, $ty->value_type);
    }, $value);
    sort($v_strs);
    return "[" . implode(", ", $v_strs) . "]";
}

function by_dict__($value, $ty) {
    $v_strs = array_map(function($key, $val) use ($ty) {
        return val_to_s__($key, $ty->key_type) . "=>" . val_to_s__($val, $ty->value_type);
    }, array_keys($value), $value);
    sort($v_strs);
    return "{" . implode(", ", $v_strs) . "}";
}

function by_option__($value, $ty) {
    if ($value === null) {
        return "null";
    } else {
        return val_to_s__($value, $ty->value_type);
    }
}

function val_to_s__($value, $ty) {
    $type_name = $ty->type_name;
    if ($type_name === "bool") {
        if (!is_bool($value)) {
            throw new Exception("Type mismatch");
        }
        return by_bool__($value);
    } elseif ($type_name === "int") {
        if (!is_int($value) && !(is_float($value) && is_int($value))) {
            throw new Exception("Type mismatch");
        }
        return by_int__($value);
    } elseif ($type_name === "double") {
        if (!is_int($value) && !is_float($value)) {
            throw new Exception("Type mismatch");
        }
        return by_double__($value);
    } elseif ($type_name === "str") {
        if (!is_string($value)) {
            throw new Exception("Type mismatch");
        }
        return by_string__($value);
    } elseif ($type_name === "list") {
        if (!is_array($value)) {
            throw new Exception("Type mismatch");
        }
        return by_list__($value, $ty);
    } elseif ($type_name === "ulist") {
        if (!is_array($value)) {
            throw new Exception("Type mismatch");
        }
        return by_ulist__($value, $ty);
    } elseif ($type_name === "dict") {
        if (!is_array($value)) {
            throw new Exception("Type mismatch");
        }
        return by_dict__($value, $ty);
    } elseif ($type_name === "option") {
        return by_option__($value, $ty);
    }
    throw new Exception("Unknown type " . $type_name);
}

function stringify__($value, $type_str) {
    return val_to_s__($value, s_to_type__($type_str)) . ":" . $type_str;
}

$tfs = stringify__(true, "bool") . "\n" .
    stringify__(3, "int") . "\n" .
    stringify__(3.141592653, "double") . "\n" .
    stringify__(3.0, "double") . "\n" .
    stringify__("Hello, World!", "str") . "\n" .
    stringify__("!@#$%^&*()\\\"\n\t", "str") . "\n" .
    stringify__([1, 2, 3], "list<int>") . "\n" .
    stringify__([true, false, true], "list<bool>") . "\n" .
    stringify__([3, 2, 1], "ulist<int>") . "\n" .
    stringify__([1 => "one", 2 => "two"], "dict<int,str>") . "\n" .
    stringify__(["one" => [1, 2, 3], "two" => [4, 5, 6]], "dict<str,list<int>>") . "\n" .
    stringify__(null, "option<int>") . "\n" .
    stringify__(3, "option<int>") . "\n";
file_put_contents("stringify.out", $tfs);