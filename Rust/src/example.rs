use std::collections::HashMap;

struct PolyEvalType {
    type_str: String,
    type_name: String,
    value_type: Option<Box<PolyEvalType>>,
    key_type: Option<Box<PolyEvalType>>,
}

fn new_poly_eval_type__(type_str: String, type_name: String, value_type: Option<Box<PolyEvalType>>, key_type: Option<Box<PolyEvalType>>) -> PolyEvalType {
    PolyEvalType {
        type_str,
        type_name,
        value_type,
        key_type,
    }
}

fn __s_to_type(type_str: &str) -> PolyEvalType {
    if !type_str.contains("<") {
        return new_poly_eval_type__(type_str.to_string(), type_str.to_string(), None, None);
    } else {
        let idx = type_str.find("<").unwrap();
        let type_name = type_str[..idx].to_string();
        let other_str = &type_str[idx + 1..type_str.len() - 1];
        if other_str.contains(",") {
            let idx = other_str.find(",").unwrap();
            let key_type = Some(Box::new(__s_to_type(&other_str[..idx])));
            let value_type = Some(Box::new(__s_to_type(&other_str[idx + 1..])));
            new_poly_eval_type__(type_str.to_string(), type_name, value_type, key_type)
        } else {
            let value_type = Some(Box::new(__s_to_type(other_str)));
            new_poly_eval_type__(type_str.to_string(), type_name, value_type, None)
        }
    }
}

fn escape_string__(s: &str) -> String {
    let mut new_s = String::new();
    for c in s.chars() {
        match c {
            '\\' => new_s.push_str("\\\\"),
            '\"' => new_s.push_str("\\\""),
            '\n' => new_s.push_str("\\n"),
            '\t' => new_s.push_str("\\t"),
            '\r' => new_s.push_str("\\r"),
            _ => new_s.push(c),
        }
    }
    new_s
}

trait MyValToS {
    fn val_to_s__(&self, t: &PolyEvalType) -> String;
}

impl MyValToS for bool {
    fn val_to_s__(&self, t: &PolyEvalType) -> String {
        if t.type_name == "bool" {
            if *self {
                "true".to_string()
            } else {
                "false".to_string()
            }
        } else {
            panic!("Type mismatch");
        }
    }
}

impl MyValToS for i32 {
    fn val_to_s__(&self, t: &PolyEvalType) -> String {
        if t.type_name == "int" {
            self.to_string()
        } else {
            panic!("Type mismatch");
        }
    }
}

impl MyValToS for f64 {
    fn val_to_s__(&self, t: &PolyEvalType) -> String {
        if t.type_name == "double" {
            let vs = format!("{:.6}", self);
            if vs.ends_with("0") {
                let vs = vs.trim_end_matches('0');
                if vs.ends_with('.') {
                    vs.to_string() + "0"
                } else {
                    vs.to_string()
                }
            } else if vs == "-0.0" {
                "0.0".to_string()
            } else {
                vs
            }
        } else {
            panic!("Type mismatch");
        }
    }
}

impl MyValToS for String {
    fn val_to_s__(&self, t: &PolyEvalType) -> String {
        if t.type_name == "str" {
            format!("\"{}\"", escape_string__(self))
        } else {
            panic!("Type mismatch");
        }
    }
}

impl<T: MyValToS> MyValToS for Vec<T> {
    fn val_to_s__(&self, t: &PolyEvalType) -> String {
        if t.type_name == "list" {
            let v_strs: Vec<String> = self.iter().map(|v| v.val_to_s__(t.value_type.as_ref().unwrap())).collect();
            format!("[{}]", v_strs.join(", "))
        } else if t.type_name == "ulist" {
            let mut v_strs: Vec<String> = self.iter().map(|v| v.val_to_s__(t.value_type.as_ref().unwrap())).collect();
            v_strs.sort();
            format!("[{}]", v_strs.join(", "))
        } else {
            panic!("Type mismatch");
        }
    }
}

impl<K: MyValToS, V: MyValToS> MyValToS for std::collections::HashMap<K, V> {
    fn val_to_s__(&self, t: &PolyEvalType) -> String {
        if t.type_name == "dict" {
            let mut v_strs: Vec<String> = self.iter().map(|(key, val)| format!("{}=>{}", key.val_to_s__(t.key_type.as_ref().unwrap()), val.val_to_s__(t.value_type.as_ref().unwrap()))).collect();
            v_strs.sort();
            format!("{{{}}}", v_strs.join(", "))
        } else {
            panic!("Type mismatch");
        }
    }
}

impl<T: MyValToS> MyValToS for Option<T> {
    fn val_to_s__(&self, t: &PolyEvalType) -> String {
        if t.type_name == "option" {
            if let Some(v) = self {
                v.val_to_s__(t.value_type.as_ref().unwrap())
            } else {
                "null".to_string()
            }
        } else {
            panic!("Type mismatch");
        }
    }
}

fn stringify__<T: MyValToS>(value: T, type_str: &str) -> String {
    value.val_to_s__(&__s_to_type(type_str)) + ":" + type_str
}

fn main() {
    let tfs = stringify__(true, "bool") + "\n" 
        + &stringify__(3, "int") + "\n" 
        + &stringify__(3.141592653, "double") + "\n" 
        + &stringify__(3.0, "double") + "\n" 
        + &stringify__("Hello, World!".to_string(), "str") + "\n" 
        + &stringify__("!@#$%^&*()\\\"\n\t".to_string(), "str") + "\n" 
        + &stringify__(vec![1, 2, 3], "list<int>") + "\n" 
        + &stringify__(vec![true, false, true], "list<bool>") + "\n" 
        + &stringify__(vec![3, 2, 1], "ulist<int>") + "\n" 
        + &stringify__(std::collections::HashMap::from([(1, "one".to_string()), (2, "two".to_string())]), "dict<int,str>") + "\n" 
        + &stringify__(std::collections::HashMap::from([("one".to_string(), vec![1, 2, 3]), ("two".to_string(), vec![4, 5, 6])]), "dict<str,list<int>>") + "\n" 
        + &stringify__(None::<i32>, "option<int>") + "\n" 
        + &stringify__(Some(3), "option<int>") + "\n";
    std::fs::write("stringify.out", tfs).unwrap();
}