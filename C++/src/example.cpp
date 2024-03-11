#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <map>
#include <cmath>
#include <ranges>
#include <optional>
#include <fstream>
#include <memory>
#include <format>

using namespace std;

class PolyEvalType;
shared_ptr<PolyEvalType> s_to_type__(string type_str);
string escape_string__(string s);
string by_bool__(bool value);
string by_int__(int value);
string by_double__(double value);
string by_string__(string value);
template <typename T> string by_list__(const vector<T>& value, shared_ptr<PolyEvalType> ty);
template <typename T> string by_ulist__(const vector<T>& value, shared_ptr<PolyEvalType> ty);
template <typename K, typename V> string by_dict__(const map<K, V>& value, shared_ptr<PolyEvalType> ty);
template <typename T> string by_option__(optional<T> value, shared_ptr<PolyEvalType> ty);
template <typename T> string val_to_s__(T value, shared_ptr<PolyEvalType> ty);
template <typename T> string val_to_s__(const vector<T>& value, shared_ptr<PolyEvalType> ty);
template <typename K, typename V> string val_to_s__(const map<K, V>& value, shared_ptr<PolyEvalType> ty);
template <typename T> string val_to_s__(optional<T> value, shared_ptr<PolyEvalType> ty);

class PolyEvalType {
public:
    string type_str;
    string type_name;
    shared_ptr<PolyEvalType> value_type;
    shared_ptr<PolyEvalType> key_type;

    PolyEvalType(string type_str, string type_name, shared_ptr<PolyEvalType> value_type, shared_ptr<PolyEvalType> key_type) {
        this->type_str = type_str;
        this->type_name = type_name;
        this->value_type = value_type;
        this->key_type = key_type;
    }
};

shared_ptr<PolyEvalType> s_to_type__(string type_str) {
    if (type_str.find("<") == string::npos) {
        return make_shared<PolyEvalType>(type_str, type_str, nullptr, nullptr);
    } else {
        int idx = type_str.find("<");
        string type_name = type_str.substr(0, idx);
        string other_str = type_str.substr(idx + 1, type_str.size() - idx - 2);
        if (other_str.find(",") == string::npos) {
            shared_ptr<PolyEvalType> value_type = s_to_type__(other_str);
            return make_shared<PolyEvalType>(type_str, type_name, value_type, nullptr);
        } else {
            idx = other_str.find(",");
            shared_ptr<PolyEvalType> key_type = s_to_type__(other_str.substr(0, idx));
            shared_ptr<PolyEvalType> value_type = s_to_type__(other_str.substr(idx + 1));
            return make_shared<PolyEvalType>(type_str, type_name, value_type, key_type);
        }
    }
}

string escape_string__(string s) {
    string new_s;
    for (char c : s) {
        if (c == '\\') {
            new_s += "\\\\";
        } else if (c == '\"') {
            new_s += "\\\"";
        } else if (c == '\n') {
            new_s += "\\n";
        } else if (c == '\t') {
            new_s += "\\t";
        } else if (c == '\r') {
            new_s += "\\r";
        } else {
            new_s += c;
        }
    }
    return new_s;
}

string by_bool__(bool value) {
    return value ? "true" : "false";
}

string by_int__(int value) {
    return to_string(value);
}

string by_double__(double value) {
    string vs = format("{:.6f}", value);
    while (vs.back() == '0') {
        vs.pop_back();
    }
    if (vs.back() == '.') {
        vs += "0";
    }
    if (vs == "-0.0") {
        vs = "0.0";
    }
    return vs;
}

string by_string__(string value) {
    return "\"" + escape_string__(value) + "\"";
}

template <typename T>
string by_list__(const vector<T>& value, shared_ptr<PolyEvalType> ty) {
    vector<string> v_strs;
    if constexpr (is_same_v<T, bool>) {
        for (bool v : value) {
            v_strs.push_back(val_to_s__((bool)v, ty->value_type));
        }
    } else {
        for (auto& v : value) {
            v_strs.push_back(val_to_s__(v, ty->value_type));
        }
    }
    string ret = "[";
    for (int i = 0; i < v_strs.size(); i++) {
        ret += v_strs[i];
        if (i < v_strs.size() - 1) {
            ret += ", ";
        }
    }
    ret += "]";
    return ret;
}

template <typename T>
string by_ulist__(const vector<T>& value, shared_ptr<PolyEvalType> ty) {
    vector<string> v_strs;
    if constexpr (is_same_v<T, bool>) {
        for (bool v : value) {
            v_strs.push_back(val_to_s__((bool)v, ty->value_type));
        }
    } else {
        for (auto& v : value) {
            v_strs.push_back(val_to_s__(v, ty->value_type));
        }
    }
    sort(v_strs.begin(), v_strs.end());
    string ret = "[";
    for (int i = 0; i < v_strs.size(); i++) {
        ret += v_strs[i];
        if (i < v_strs.size() - 1) {
            ret += ", ";
        }
    }
    ret += "]";
    return ret;
}

template <typename K, typename V>
string by_dict__(const map<K, V>& value, shared_ptr<PolyEvalType> ty) {
    vector<string> v_strs;
    for (const auto& [key, val] : value) {
        v_strs.push_back(val_to_s__(key, ty->key_type) + "=>" + val_to_s__(val, ty->value_type));
    }
    sort(v_strs.begin(), v_strs.end());
    string ret = "{";
    for (int i = 0; i < v_strs.size(); i++) {
        ret += v_strs[i];
        if (i < v_strs.size() - 1) {
            ret += ", ";
        }
    }
    ret += "}";
    return ret;
}

template <typename T>
string by_option__(optional<T> value, shared_ptr<PolyEvalType> ty) {
    if (!value.has_value()) {
        return "null";
    } else {
        return val_to_s__(value.value(), ty->value_type);
    }
}

template <typename T>
string val_to_s__(T value, shared_ptr<PolyEvalType> ty) {
    string type_name = ty->type_name;
    if (type_name == "bool") {
        if constexpr (is_same_v<T, bool>) {
            return by_bool__(value);
        } else {
            throw invalid_argument("Type mismatch");
        }
    } else if (type_name == "int") {
        if constexpr (is_same_v<T, int>) {
            return by_int__(value);
        } else {
            throw invalid_argument("Type mismatch");
        }
    } else if (type_name == "double") {
        if constexpr (is_same_v<T, double>) {
            return by_double__(value);
        } else {
            throw invalid_argument("Type mismatch");
        }
    } else if (type_name == "str") {
        if constexpr (is_same_v<T, string>) {
            return by_string__(value);
        } else {
            throw invalid_argument("Type mismatch");
        }
    }
    throw invalid_argument("Unknown type " + type_name);
}

template <typename T>
string val_to_s__(const vector<T>& value, shared_ptr<PolyEvalType> ty) {
    if (ty->type_name == "list") {
        return by_list__(value, ty);
    } else if (ty->type_name == "ulist") {
        return by_ulist__(value, ty);
    }
    throw invalid_argument("Unknown type " + ty->type_name);
}

template <typename K, typename V>
string val_to_s__(const map<K, V>& value, shared_ptr<PolyEvalType> ty) {
    if (ty->type_name == "dict") {
        return by_dict__(value, ty);
    }
    throw invalid_argument("Unknown type " + ty->type_name);
}

template <typename T>
string val_to_s__(optional<T> value, shared_ptr<PolyEvalType> ty) {
    if (ty->type_name == "option") {
        return by_option__(value, ty);
    }
    throw invalid_argument("Unknown type " + ty->type_name);
}


template <typename T>
string stringify__(T value, string type_str) {
    return val_to_s__(value, s_to_type__(type_str)) + ":" + type_str;
}

template <typename T>
string stringify__(const vector<T>& value, string type_str) {
    return val_to_s__(value, s_to_type__(type_str)) + ":" + type_str;
}

template <typename K, typename V>
string stringify__(const map<K, V>& value, string type_str) {
    return val_to_s__(value, s_to_type__(type_str)) + ":" + type_str;
}


template <typename T>
string stringify__(optional<T> value, string type_str) {
    return by_option__(value, s_to_type__(type_str)) + ":" + type_str;
}

int main() {
    string tfs = stringify__(true, "bool") + "\n"
        + stringify__(3, "int") + "\n"
        + stringify__(3.141592653, "double") + "\n"
        + stringify__(3.0, "double") + "\n"
        + stringify__(string("Hello, World!"), "str") + "\n"
        + stringify__(string("!@#$%^&*()\\\"\n\t"), "str") + "\n"
        + stringify__(vector<int>{1, 2, 3}, "list<int>") + "\n"
        + stringify__(vector<bool>{true, false, true}, "list<bool>") + "\n"
        + stringify__(vector<int>{3, 2, 1}, "ulist<int>") + "\n"
        + stringify__(map<int, string>{{1, "one"}, {2, "two"}}, "dict<int,str>") + "\n"
        + stringify__(map<string, vector<int>>{{"one", {1, 2, 3}}, {"two", {4, 5, 6}}}, "dict<str,list<int>>") + "\n"
        + stringify__(optional<int>(), "option<int>") + "\n"
        + stringify__(optional<int>(3), "option<int>") + "\n";
    ofstream f("stringify.out");
    f << tfs;
    f.close();
}