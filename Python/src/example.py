from __future__ import annotations
from typing import Optional
import math

class PolyEvalType:
    def __init__(self, type_str: str, type_name: str, value_type: Optional[PolyEvalType], key_type: Optional[PolyEvalType]):
        self.type_str = type_str
        self.type_name = type_name
        self.value_type = value_type
        self.key_type = key_type

def __s_to_type(type_str: str) -> PolyEvalType:
    if "<" not in type_str:
        return PolyEvalType(type_str, type_str, None, None)
    else:
        idx = type_str.index("<")
        type_name = type_str[:idx]
        other_str = type_str[idx + 1:-1]
        if "," not in other_str:
            value_type = __s_to_type(other_str)
            return PolyEvalType(type_str, type_name, value_type, None)
        else:
            idx = other_str.index(",")
            key_type = __s_to_type(other_str[:idx])
            value_type = __s_to_type(other_str[idx + 1:])
            return PolyEvalType(type_str, type_name, value_type, key_type)

def __escape_string(s: str):
    new_s = []
    for c in s:
        if c == "\\":
            new_s.append("\\\\")
        elif c == "\"":
            new_s.append("\\\"")
        elif c == "\n":
            new_s.append("\\n")
        elif c == "\t":
            new_s.append("\\t")
        elif c == "\r":
            new_s.append("\\r")
        else:
            new_s.append(c)
    return "".join(new_s)


def __by_bool(value):
    return "true" if value else "false"


def __by_int(value):
    v = int(value)
    return str(v)


def __by_double(value):
    v = float(value)
    vs = "{:.6f}".format(v)
    while vs.endswith("0"):
        vs = vs[:-1]
    if vs.endswith("."):
        vs += "0"
    if vs == "-0.0":
        vs = "0.0"
    return vs


def __by_string(value):
    return '"' + __escape_string(value) + '"'


def __by_list(value, ty: PolyEvalType):
    v_strs = [__val_to_s(v, ty.value_type) for v in value]
    return "[" + ", ".join(v_strs) + "]"


def __by_ulist(value, ty: PolyEvalType):
    v_strs = [__val_to_s(v, ty.value_type) for v in value]
    return "[" + ", ".join(sorted(v_strs)) + "]"


def __by_dict(value, ty: PolyEvalType):
    v_strs = [__val_to_s(key, ty.key_type) + "=>" + __val_to_s(val, ty.value_type) for key, val in value.items()]
    return "{" + ", ".join(sorted(v_strs)) + "}"


def __by_option(value, ty: PolyEvalType):
    if value is None:
        return "null"
    else:
        return __val_to_s(value, ty.value_type)


def __val_to_s(value, ty: PolyEvalType):
    type_name = ty.type_name
    if type_name == "bool":
        if not isinstance(value, bool):
            raise ValueError("Type mismatch")
        return __by_bool(value)
    elif type_name == "int":
        if not isinstance(value, int) and not (isinstance(value, float) and value.is_integer()):
            raise ValueError("Type mismatch")
        return __by_int(value)
    elif type_name == "double":
        if not isinstance(value, int) and not isinstance(value, float):
            raise ValueError("Type mismatch")
        return __by_double(value)
    elif type_name == "str":
        if not isinstance(value, str):
            raise ValueError("Type mismatch")
        return __by_string(value)
    elif type_name == "list":
        if not isinstance(value, list):
            raise ValueError("Type mismatch")
        return __by_list(value, ty)
    elif type_name == "ulist":
        if not isinstance(value, list):
            raise ValueError("Type mismatch")
        return __by_ulist(value, ty)
    elif type_name == "dict":
        if not isinstance(value, dict):
            raise ValueError("Type mismatch")
        return __by_dict(value, ty)
    elif type_name == "option":
        return __by_option(value, ty)
    raise ValueError(f"Unknown type {type_name}")


def __stringify(value, type_str: str):
    return __val_to_s(value, __s_to_type(type_str)) + ":" + type_str

tfs = __stringify(True, "bool") + "\n" \
    + __stringify(3, "int") + "\n" \
    + __stringify(3.141592653, "double") + "\n" \
    + __stringify(3.0, "double") + "\n" \
    + __stringify("Hello, World!", "str") + "\n" \
    + __stringify("!@#$%^&*()\\\"\n\t", "str") + "\n" \
    + __stringify([1, 2, 3], "list<int>") + "\n" \
    + __stringify([True, False, True], "list<bool>") + "\n" \
    + __stringify([3, 2, 1], "ulist<int>") + "\n" \
    + __stringify({1: "one", 2: "two"}, "dict<int,str>") + "\n" \
    + __stringify({"one": [1, 2, 3], "two": [4, 5, 6]}, "dict<str,list<int>>") + "\n" \
    + __stringify(None, "option<int>") + "\n" \
    + __stringify(3, "option<int>") + "\n"
with open("stringify.out", "w") as f:
    f.write(tfs)
