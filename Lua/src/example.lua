local PolyEvalType = {}
PolyEvalType.__index = PolyEvalType
function PolyEvalType.new(type_str, type_name, value_type, key_type)
    local self = setmetatable({}, PolyEvalType)
    self.type_str = type_str
    self.type_name = type_name
    self.value_type = value_type
    self.key_type = key_type
    return self
end

local function s_to_type__(type_str)
    if not string.find(type_str, "<", 1, true) then
        return PolyEvalType.new(type_str, type_str, nil, nil)
    else
        local idx = string.find(type_str, "<", 1, true)
        local type_name = string.sub(type_str, 1, idx - 1)
        local other_str = string.sub(type_str, idx + 1, -2)
        if not string.find(other_str, ",", 1, true) then
            local value_type = s_to_type__(other_str)
            return PolyEvalType.new(type_str, type_name, value_type, nil)
        else
            local idx = string.find(other_str, ",", 1, true)
            local key_type = s_to_type__(string.sub(other_str, 1, idx - 1))
            local value_type = s_to_type__(string.sub(other_str, idx + 1))
            return PolyEvalType.new(type_str, type_name, value_type, key_type)
        end
    end
end

local val_to_s__

local function escape_string__(s)
    local new_s = {}
    for i = 1, #s do
        local c = string.sub(s, i, i)
        if c == "\\" then
            table.insert(new_s, "\\\\")
        elseif c == "\"" then
            table.insert(new_s, "\\\"")
        elseif c == "\n" then
            table.insert(new_s, "\\n")
        elseif c == "\t" then
            table.insert(new_s, "\\t")
        elseif c == "\r" then
            table.insert(new_s, "\\r")
        else
            table.insert(new_s, c)
        end
    end
    return table.concat(new_s)
end

local function by_bool__(value)
    return value and "true" or "false"
end

local function by_int__(value)
    return tostring(value)
end

local function by_double__(value)
    local vs = string.format("%.6f", value)
    while string.sub(vs, -1) == "0" do
        vs = string.sub(vs, 1, -2)
    end
    if string.sub(vs, -1) == "." then
        vs = vs .. "0"
    end
    if vs == "-0.0" then
        vs = "0.0"
    end
    return vs
end

local function by_string__(value)
    return '"' .. escape_string__(value) .. '"'
end

local function by_list__(value, ty)
    local v_strs = {}
    for _, v in ipairs(value) do
        table.insert(v_strs, val_to_s__(v, ty.value_type))
    end
    return "[" .. table.concat(v_strs, ", ") .. "]"
end

local function by_ulist__(value, ty)
    local v_strs = {}
    for _, v in ipairs(value) do
        table.insert(v_strs, val_to_s__(v, ty.value_type))
    end
    table.sort(v_strs)
    return "[" .. table.concat(v_strs, ", ") .. "]"
end

local function by_dict__(value, ty)
    local v_strs = {}
    for key, val in pairs(value) do
        table.insert(v_strs, val_to_s__(key, ty.key_type) .. "=>" .. val_to_s__(val, ty.value_type))
    end
    table.sort(v_strs)
    return "{" .. table.concat(v_strs, ", ") .. "}"
end

local function by_option__(value, ty)
    if value == nil then
        return "null"
    else
        return val_to_s__(value, ty.value_type)
    end
end

function val_to_s__(value, ty)
    local type_name = ty.type_name
    if type_name == "bool" then
        if type(value) ~= "boolean" then
            error("Type mismatch")
        end
        return by_bool__(value)
    elseif type_name == "int" then
        if type(value) ~= "number" or math.floor(value) ~= value then
            error("Type mismatch")
        end
        return by_int__(value)
    elseif type_name == "double" then
        if type(value) ~= "number" then
            error("Type mismatch")
        end
        return by_double__(value)
    elseif type_name == "str" then
        if type(value) ~= "string" then
            error("Type mismatch")
        end
        return by_string__(value)
    elseif type_name == "list" then
        if type(value) ~= "table" then
            error("Type mismatch")
        end
        return by_list__(value, ty)
    elseif type_name == "ulist" then
        if type(value) ~= "table" then
            error("Type mismatch")
        end
        return by_ulist__(value, ty)
    elseif type_name == "dict" then
        if type(value) ~= "table" then
            error("Type mismatch")
        end
        return by_dict__(value, ty)
    elseif type_name == "option" then
        return by_option__(value, ty)
    end
    error("Unknown type " .. type_name)
end

local function stringify__(value, type_str)
    return val_to_s__(value, s_to_type__(type_str)) .. ":" .. type_str
end

local tfs = stringify__(true, "bool") .. "\n" ..
    stringify__(3, "int") .. "\n" ..
    stringify__(3.141592653, "double") .. "\n" ..
    stringify__(3.0, "double") .. "\n" ..
    stringify__("Hello, World!", "str") .. "\n" ..
    stringify__("!@#$%^&*()\\\"\n\t", "str") .. "\n" ..
    stringify__({1, 2, 3}, "list<int>") .. "\n" ..
    stringify__({true, false, true}, "list<bool>") .. "\n" ..
    stringify__({3, 2, 1}, "ulist<int>") .. "\n" ..
    stringify__({[1] = "one", [2] = "two"}, "dict<int,str>") .. "\n" ..
    stringify__({["one"] = {1, 2, 3}, ["two"] = {4, 5, 6}}, "dict<str,list<int>>") .. "\n" ..
    stringify__(nil, "option<int>") .. "\n" ..
    stringify__(3, "option<int>") .. "\n"
local f = io.open("stringify.out", "w")
f:write(tfs)
f:close()