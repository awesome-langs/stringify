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
shared_ptr<PolyEvalType> __s_to_type(string type_str);
string __escape_string(string s);
string __by_bool(bool value);
string __by_int(int value);
string __by_double(double value);
string __by_string(string value);
template <typename T> string __by_list(const vector<T>& value, shared_ptr<PolyEvalType> ty);
template <typename T> string __by_ulist(const vector<T>& value, shared_ptr<PolyEvalType> ty);
template <typename K, typename V> string __by_dict(const map<K, V>& value, shared_ptr<PolyEvalType> ty);
template <typename T> string __by_option(optional<T> value, shared_ptr<PolyEvalType> ty);
template <typename T> string __val_to_s(T value, shared_ptr<PolyEvalType> ty);
template <typename T> string __val_to_s(const vector<T>& value, shared_ptr<PolyEvalType> ty);
template <typename K, typename V> string __val_to_s(const map<K, V>& value, shared_ptr<PolyEvalType> ty);
template <typename T> string __val_to_s(optional<T> value, shared_ptr<PolyEvalType> ty);

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

shared_ptr<PolyEvalType> __s_to_type(string type_str) {
    if (type_str.find("<") == string::npos) {
        return make_shared<PolyEvalType>(type_str, type_str, nullptr, nullptr);
    } else {
        int idx = type_str.find("<");
        string type_name = type_str.substr(0, idx);
        string other_str = type_str.substr(idx + 1, type_str.size() - idx - 2);
        if (other_str.find(",") == string::npos) {
            shared_ptr<PolyEvalType> value_type = __s_to_type(other_str);
            return make_shared<PolyEvalType>(type_str, type_name, value_type, nullptr);
        } else {
            idx = other_str.find(",");
            shared_ptr<PolyEvalType> key_type = __s_to_type(other_str.substr(0, idx));
            shared_ptr<PolyEvalType> value_type = __s_to_type(other_str.substr(idx + 1));
            return make_shared<PolyEvalType>(type_str, type_name, value_type, key_type);
        }
    }
}

string __escape_string(string s) {
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

string __by_bool(bool value) {
    return value ? "true" : "false";
}

string __by_int(int value) {
    return to_string(value);
}

string __by_double(double value) {
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

string __by_string(string value) {
    return "\"" + __escape_string(value) + "\"";
}

template <typename T>
string __by_list(const vector<T>& value, shared_ptr<PolyEvalType> ty) {
    vector<string> v_strs;
    if constexpr (is_same_v<T, bool>) {
        for (bool v : value) {
            v_strs.push_back(__val_to_s((bool)v, ty->value_type));
        }
    } else {
        for (auto& v : value) {
            v_strs.push_back(__val_to_s(v, ty->value_type));
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
string __by_ulist(const vector<T>& value, shared_ptr<PolyEvalType> ty) {
    vector<string> v_strs;
    if constexpr (is_same_v<T, bool>) {
        for (bool v : value) {
            v_strs.push_back(__val_to_s((bool)v, ty->value_type));
        }
    } else {
        for (auto& v : value) {
            v_strs.push_back(__val_to_s(v, ty->value_type));
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
string __by_dict(const map<K, V>& value, shared_ptr<PolyEvalType> ty) {
    vector<string> v_strs;
    for (const auto& [key, val] : value) {
        v_strs.push_back(__val_to_s(key, ty->key_type) + "=>" + __val_to_s(val, ty->value_type));
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
string __by_option(optional<T> value, shared_ptr<PolyEvalType> ty) {
    if (!value.has_value()) {
        return "null";
    } else {
        return __val_to_s(value.value(), ty->value_type);
    }
}

template <typename T>
string __val_to_s(T value, shared_ptr<PolyEvalType> ty) {
    string type_name = ty->type_name;
    if (type_name == "bool") {
        if constexpr (is_same_v<T, bool>) {
            return __by_bool(value);
        } else {
            throw invalid_argument("Type mismatch");
        }
    } else if (type_name == "int") {
        if constexpr (is_same_v<T, int>) {
            return __by_int(value);
        } else {
            throw invalid_argument("Type mismatch");
        }
    } else if (type_name == "double") {
        if constexpr (is_same_v<T, double>) {
            return __by_double(value);
        } else {
            throw invalid_argument("Type mismatch");
        }
    } else if (type_name == "str") {
        if constexpr (is_same_v<T, string>) {
            return __by_string(value);
        } else {
            throw invalid_argument("Type mismatch");
        }
    }
    throw invalid_argument("Unknown type " + type_name);
}

template <typename T>
string __val_to_s(const vector<T>& value, shared_ptr<PolyEvalType> ty) {
    if (ty->type_name == "list") {
        return __by_list(value, ty);
    } else if (ty->type_name == "ulist") {
        return __by_ulist(value, ty);
    }
    throw invalid_argument("Unknown type " + ty->type_name);
}

template <typename K, typename V>
string __val_to_s(const map<K, V>& value, shared_ptr<PolyEvalType> ty) {
    if (ty->type_name == "dict") {
        return __by_dict(value, ty);
    }
    throw invalid_argument("Unknown type " + ty->type_name);
}

template <typename T>
string __val_to_s(optional<T> value, shared_ptr<PolyEvalType> ty) {
    if (ty->type_name == "option") {
        return __by_option(value, ty);
    }
    throw invalid_argument("Unknown type " + ty->type_name);
}


template <typename T>
string __stringify(T value, string type_str) {
    return __val_to_s(value, __s_to_type(type_str)) + ":" + type_str;
}

template <typename T>
string __stringify(const vector<T>& value, string type_str) {
    return __val_to_s(value, __s_to_type(type_str)) + ":" + type_str;
}

template <typename K, typename V>
string __stringify(const map<K, V>& value, string type_str) {
    return __val_to_s(value, __s_to_type(type_str)) + ":" + type_str;
}


template <typename T>
string __stringify(optional<T> value, string type_str) {
    return __by_option(value, __s_to_type(type_str)) + ":" + type_str;
}

int main() {
    string tfs = __stringify(true, "bool") + "\n"
        + __stringify(3, "int") + "\n"
        + __stringify(3.141592653, "double") + "\n"
        + __stringify(3.0, "double") + "\n"
        + __stringify(string("Hello, World!"), "str") + "\n"
        + __stringify(string("!@#$%^&*()\\\"\n\t"), "str") + "\n"
        + __stringify(vector<int>{1, 2, 3}, "list<int>") + "\n"
        + __stringify(vector<bool>{true, false, true}, "list<bool>") + "\n"
        + __stringify(vector<int>{3, 2, 1}, "ulist<int>") + "\n"
        + __stringify(map<int, string>{{1, "one"}, {2, "two"}}, "dict<int,str>") + "\n"
        + __stringify(map<string, vector<int>>{{"one", {1, 2, 3}}, {"two", {4, 5, 6}}}, "dict<str,list<int>>") + "\n"
        + __stringify(optional<int>(), "option<int>") + "\n"
        + __stringify(optional<int>(3), "option<int>") + "\n";
    ofstream f("stringify.out");
    f << tfs;
    f.close();
}