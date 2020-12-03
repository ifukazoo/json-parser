use super::parser::*;
use std::collections::HashMap;

pub fn print(value: &JSONValue) -> String {
    match value {
        JSONValue::Num(n) => print_num(*n),
        JSONValue::Bool(b) => print_bool(*b),
        JSONValue::Null => print_null(),
        JSONValue::Str(s) => print_string(&s),
        JSONValue::Array(a) => print_array(&a),
        JSONValue::Object(h) => print_obj(&h),
    }
}
fn print_num(n: f64) -> String {
    format!("{}", n)
}
fn print_bool(b: bool) -> String {
    format!("{}", b)
}
fn print_null() -> String {
    "null".to_string()
}
fn print_string(s: &str) -> String {
    format!("\"{}\"", s)
}
fn print_array(a: &Vec<JSONValue>) -> String {
    let mut buf = String::new();
    let mut sep = String::from("");
    buf.push_str("[");
    for v in a {
        buf.push_str(&sep);
        sep = ",".to_string();
        buf.push_str(&print(v));
    }
    buf.push_str("]");
    buf
}
fn print_obj(h: &HashMap<String, JSONValue>) -> String {
    let mut buf = String::new();
    let mut sep = String::from("");
    buf.push_str("{");
    for (k, v) in h {
        buf.push_str(&sep);
        sep = ",".to_string();
        buf.push_str(&format!("\"{}\":", k));
        buf.push_str(&print(v));
    }
    buf.push_str("}");
    buf
}

#[cfg(test)]
mod test {
    use super::super::parser;
    use super::super::printer;

    #[test]
    fn test_print_num() {
        let inputs = vec!["12.3", "true", "false", "null", r#" [1, "str", true] "#];
        let expected = vec!["12.3", "true", "false", "null", "[1,\"str\",true]"];

        for (i, _) in inputs.iter().enumerate() {
            let json = inputs[i].parse::<parser::JSONValue>().unwrap();
            let s = printer::print(&json);
            assert_eq!(s, expected[i]);
        }
    }
}
