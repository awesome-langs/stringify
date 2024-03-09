use std::collections::HashMap;

fn my_string_to_int(s: &String) -> i32 {
    s.parse::<i32>().unwrap()
}

fn my_string_to_double(s: &String) -> f64 {
    s.parse::<f64>().unwrap()
}

fn my_int_to_string(i: i32) -> String {
    i.to_string()
}

fn my_double_to_string(d: f64) -> String {
    format!("{:.6}", d)
}

fn my_bool_to_string(b: bool) -> String {
    if b { "true" } else { "false" }.to_string()
}

fn my_int_to_nullable(i: i32) -> Option<i32> {
    if i > 0 {
        Some(i)
    } else if i < 0 {
        Some(-i)
    } else {
        None
    }
}

fn my_nullable_to_int(i: &Option<i32>) -> i32 {
    match i {
        Some(x) => x,
        None => -1,
    }
}

fn my_list_sorted(lst: &Vec<String>) -> Vec<String> {
    let mut tmp = lst.clone();
    tmp.sort();
    tmp
}

fn my_list_sorted_by_length(lst: &Vec<String>) -> Vec<String> {
    let mut tmp = lst.clone();
    tmp.sort_by(|a, b| a.len().cmp(&b.len()));
    tmp
}

fn my_list_filter(lst: &Vec<i32>) -> Vec<i32> {
    lst.iter().filter(|&x| x % 3 == 0).cloned().collect()
}

fn my_list_map(lst: &Vec<i32>) -> Vec<i32> {
    lst.iter().map(|x| x * x).collect()
}

fn my_list_reduce(lst: &Vec<i32>) -> i32 {
    lst.iter().fold(0, |acc, &x| acc * 10 + x)
}

fn my_list_operations(lst: &Vec<i32>) -> i32 {
    lst.iter().filter(|&x| x % 3 == 0)
        .map(|x| x * x)
        .fold(0, |acc, x| acc * 10 + x)
}

fn my_list_to_dict(lst: &Vec<i32>) -> HashMap<i32, i32> {
    lst.iter().map(|&x| (x, x * x)).collect()
}

fn my_dict_to_list(dict: &HashMap<i32, i32>) -> Vec<i32> {
    let mut tmp: Vec<(i32, i32)> = dict.iter().map(|(&k, &v)| (k, v)).collect();
    tmp.sort_by(|a, b| a.0.cmp(&b.0));
    tmp.iter().map(|(k, v)| k + v).collect()
}

fn my_print_string(s: &String) {
    println!("{}", s);
}

fn my_print_string_list(lst: &Vec<String>) {
    for x in lst {
        print!("{}", x.to_string() + " ");
    }
    println!();
}

fn my_print_int_list(lst: &Vec<i32>) {
    my_print_string_list(&lst.iter().map(|x| my_int_to_string(*x)).collect());
}

fn my_print_dict(dict: &HashMap<i32, i32>) {
    for (k, v) in dict {
        print!("{}", my_int_to_string(*k) + "->" + &my_int_to_string(*v) + " ");
    }
    println!();
}

fn main() {
    my_print_string(&"Hello, World!".to_string());
    my_print_string(&my_int_to_string(my_string_to_int(&"123".to_string())));
    my_print_string(&my_double_to_string(my_string_to_double(&"123.456".to_string())));
    my_print_string(&my_bool_to_string(false));
    my_print_string(&my_int_to_string(my_nullable_to_int(my_int_to_nullable(18))));
    my_print_string(&my_int_to_string(my_nullable_to_int(my_int_to_nullable(-15))));
    my_print_string(&my_int_to_string(my_nullable_to_int(my_int_to_nullable(0))));
    my_print_string_list(&my_list_sorted(&vec!["e".to_string(), "dddd".to_string(), "ccccc".to_string(), "bb".to_string(), "aaa".to_string()]));
    my_print_string_list(&my_list_sorted_by_length(&vec!["e".to_string(), "dddd".to_string(), "ccccc".to_string(), "bb".to_string(), "aaa".to_string()]));
    my_print_string(&my_int_to_string(my_list_reduce(&my_list_map(&my_list_filter(&vec![3, 12, 5, 8, 9, 15, 7, 17, 21, 11])))));
    my_print_string(&my_int_to_string(my_list_operations(&vec![3, 12, 5, 8, 9, 15, 7, 17, 21, 11])));
    my_print_dict(&my_list_to_dict(&vec![3, 1, 4, 2, 5, 9, 8, 6, 7, 0]));
    my_print_int_list(&my_dict_to_list(&[(3, 9), (1, 1), (4, 16), (2, 4), (5, 25), (9, 81), (8, 64), (6, 36), (7, 49), (0, 0)].iter().cloned().collect()));
}