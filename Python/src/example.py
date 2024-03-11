from __future__ import annotations
from typing import Optional
import math

class PolyEvalType:
    def __init__(self, type_str: str, type_name: str, value_type: Optional[PolyEvalType], key_type: Optional[PolyEvalType]):
        self.type_str = type_str
        self.type_name = type_name
        self.value_type = value_type
        self.key_type = key_type

def s_to_type__(type_str: str) -> PolyEvalType:
    if "<" not in type_str:
        return PolyEvalType(type_str, type_str, None, None)
    else:
        idx = type_str.index("<")
        type_name = type_str[:idx]
        other_str = type_str[idx + 1:-1]
        if "," not in other_str:
            value_type = s_to_type__(other_str)
            return PolyEvalType(type_str, type_name, value_type, None)
        else:
            idx = other_str.index(",")
            key_type = s_to_type__(other_str[:idx])
            value_type = s_to_type__(other_str[idx + 1:])
            return PolyEvalType(type_str, type_name, value_type, key_type)

def escape_string__(s: str):
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


def by_bool__(value):
    return "true" if value else "false"


def by_int__(value):
    v = int(value)
    return str(v)


def by_double__(value):
    v = float(value)
    vs = "{:.6f}".format(v)
    while vs.endswith("0"):
        vs = vs[:-1]
    if vs.endswith("."):
        vs += "0"
    if vs == "-0.0":
        vs = "0.0"
    return vs


def by_string__(value):
    return '"' + escape_string__(value) + '"'


def by_list__(value, ty: PolyEvalType):
    v_strs = [val_to_s__(v, ty.value_type) for v in value]
    return "[" + ", ".join(v_strs) + "]"


def by_ulist__(value, ty: PolyEvalType):
    v_strs = [val_to_s__(v, ty.value_type) for v in value]
    return "[" + ", ".join(sorted(v_strs)) + "]"


def by_dict__(value, ty: PolyEvalType):
    v_strs = [val_to_s__(key, ty.key_type) + "=>" + val_to_s__(val, ty.value_type) for key, val in value.items()]
    return "{" + ", ".join(sorted(v_strs)) + "}"


def by_option__(value, ty: PolyEvalType):
    if value is None:
        return "null"
    else:
        return val_to_s__(value, ty.value_type)


def val_to_s__(value, ty: PolyEvalType):
    type_name = ty.type_name
    if type_name == "bool":
        if not isinstance(value, bool):
            raise ValueError("Type mismatch")
        return by_bool__(value)
    elif type_name == "int":
        if not isinstance(value, int) and not (isinstance(value, float) and value.is_integer()):
            raise ValueError("Type mismatch")
        return by_int__(value)
    elif type_name == "double":
        if not isinstance(value, int) and not isinstance(value, float):
            raise ValueError("Type mismatch")
        return by_double__(value)
    elif type_name == "str":
        if not isinstance(value, str):
            raise ValueError("Type mismatch")
        return by_string__(value)
    elif type_name == "list":
        if not isinstance(value, list):
            raise ValueError("Type mismatch")
        return by_list__(value, ty)
    elif type_name == "ulist":
        if not isinstance(value, list):
            raise ValueError("Type mismatch")
        return by_ulist__(value, ty)
    elif type_name == "dict":
        if not isinstance(value, dict):
            raise ValueError("Type mismatch")
        return by_dict__(value, ty)
    elif type_name == "option":
        return by_option__(value, ty)
    raise ValueError(f"Unknown type {type_name}")


def stringify__(value, type_str: str):
    return val_to_s__(value, s_to_type__(type_str)) + ":" + type_str

tfs = stringify__(True, "bool") + "\n" \
    + stringify__(3, "int") + "\n" \
    + stringify__(3.141592653, "double") + "\n" \
    + stringify__(3.0, "double") + "\n" \
    + stringify__("Hello, World!", "str") + "\n" \
    + stringify__("!@#$%^&*()\\\"\n\t", "str") + "\n" \
    + stringify__([1, 2, 3], "list<int>") + "\n" \
    + stringify__([True, False, True], "list<bool>") + "\n" \
    + stringify__([3, 2, 1], "ulist<int>") + "\n" \
    + stringify__({1: "one", 2: "two"}, "dict<int,str>") + "\n" \
    + stringify__({"one": [1, 2, 3], "two": [4, 5, 6]}, "dict<str,list<int>>") + "\n" \
    + stringify__(None, "option<int>") + "\n" \
    + stringify__(3, "option<int>") + "\n"
with open("stringify.out", "w") as f:
    f.write(tfs)
