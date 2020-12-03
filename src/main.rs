use json_parser::lexer;
use json_parser::parser;
use json_parser::printer;
use std::io;
use std::io::Read;

fn main() {
    let stdin = io::stdin();
    let stdin = stdin.lock();
    let mut stdin = io::BufReader::new(stdin);

    let mut buf = String::new();
    stdin.read_to_string(&mut buf).unwrap();

    match parser::parse(lexer::lex(&buf)) {
        Ok(value) => {
            println!("{:?}", value);
            println!("{}", printer::print(&value));
        }
        Err(err) => println!("{:?}", err),
    }
}
