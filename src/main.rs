use json_parser::lexer;
use json_parser::parser;
use std::io;
use std::io::Read;

fn main() {
    let stdin = io::stdin();
    let stdin = stdin.lock();
    let mut stdin = io::BufReader::new(stdin);

    let mut buf = String::new();
    stdin.read_to_string(&mut buf).unwrap();

    let (tokens, input) = lexer::lex(&buf);
    for elem in &tokens {
        println!("{:?}", elem);
    }
    let value = parser::parse(tokens, &input);
    println!("{:?}", value);
}
