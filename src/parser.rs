use super::lexer;
use super::lexer::LexResult;
use super::lexer::Token;
use super::lexer::TokenKind;
use std::collections::HashMap;
use std::iter::Peekable;
use std::str::FromStr;

/// JSON値
#[derive(Debug, Clone, PartialEq)]
pub enum JSONValue {
    Str(String),
    Num(f64),
    Bool(bool),
    Object(HashMap<String, JSONValue>),
    Array(Vec<JSONValue>),
    Null,
}

impl FromStr for JSONValue {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse(lexer::lex(s))
    }
}

/// パースエラー
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    /// 不正なトークン
    IllegalToken(Token),
    /// 予期していないトークン
    UnexpectedToken(Token),
    /// 閉じていないカッコ
    UnclosedParenOrBrace,
    /// 予期せぬ入力終了
    UnexpectedEOF,
}

/// lexで生成したtokenとcharスライスからJSON値を生成する
///
/// Examples
/// ```
///use std::collections::HashMap;
///use json_parser::lexer;
///use json_parser::parser;
///use parser::JSONValue;
///
///let input = r#"{
///    "key":1.0
///}"#;
///let value = input.parse::<parser::JSONValue>();
///assert_eq!(
///    Ok(JSONValue::Object(
///        [("key".to_string(), JSONValue::Num(1f64))]
///            .iter()
///            .cloned()
///            .collect::<HashMap<String, JSONValue>>()
///    )),
///    value
///);
/// ```
///
fn parse(lex_result: LexResult) -> Result<JSONValue, ParseError> {
    let tokens = lex_result;
    let mut peekable = tokens.into_iter().peekable();
    parse_value(&mut peekable)
}

fn parse_value<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<JSONValue, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    use TokenKind::*;

    match tokens.peek().map(|token| &token.kind) {
        Some(Digits) | Some(Minus) => parse_number(tokens),
        Some(CharSeq) => parse_charseq(tokens),
        Some(StrBody) => parse_string(tokens),
        Some(LBracket) => parse_array(tokens),
        Some(LBrace) => parse_obj(tokens),
        // RBracket,  RBrace, Comma, Colon, => NG
        Some(_) => Err(ParseError::UnexpectedToken(tokens.next().unwrap())),
        // 予期せぬ終了
        None => Err(ParseError::UnexpectedEOF),
    }
}

fn parse_number<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<JSONValue, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    use TokenKind::*;
    let mut sign_minus = false;

    // 符号の刈り取り
    if let Some(kind) = tokens.peek().map(|token| &token.kind) {
        if *kind == Minus {
            sign_minus = true;
            tokens.next().unwrap();
        }
    }

    // 整数部
    let token = expect_peek(tokens, Digits)?;
    let integer = extra_literal(&token);
    let integer = if integer.starts_with("0") {
        if integer.len() != 1 {
            // 01, 012, .. => NG
            return Err(ParseError::IllegalToken(token));
        }
        0
    } else {
        integer.parse::<u64>().unwrap()
    };

    // 小数部
    let fraction = match tokens.peek().map(|token| &token.kind) {
        Some(Dot) => {
            expect_peek(tokens, Dot)?; // "`.`刈り取り"
            let token = expect_peek(tokens, Digits)?; // `.`のあとを刈り取り
            let f = extra_literal(&token);
            let f = format!("0.{}", f);
            f.parse::<f64>().unwrap()
        }
        // 小数部なし
        _ => 0.0,
    };

    let value = integer as f64 + fraction;
    let value = if sign_minus { value * -1.0 } else { value };
    Ok(JSONValue::Num(value as f64))
}

fn parse_charseq<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<JSONValue, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    use TokenKind::*;

    let token = expect_peek(tokens, CharSeq)?;
    let char_seq = extra_literal(&token);
    match char_seq.as_ref() {
        "true" => Ok(JSONValue::Bool(true)),
        "false" => Ok(JSONValue::Bool(false)),
        "null" => Ok(JSONValue::Null),
        _ => Err(ParseError::UnexpectedToken(token)),
    }
}

fn parse_string<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<JSONValue, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    use TokenKind::*;

    let token = expect_peek(tokens, StrBody)?;
    let s = extra_literal(&token);
    // 両端の`"`を除去
    Ok(JSONValue::Str(String::from(s.trim_matches('"'))))
}

fn parse_array<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<JSONValue, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    use TokenKind::*;

    // 最初の `[` を刈り取り
    expect_peek(tokens, LBracket)?;

    match tokens.peek().map(|token| &token.kind) {
        // 閉じてない
        None => Err(ParseError::UnclosedParenOrBrace),
        // 空のArray
        Some(RBracket) => Ok(JSONValue::Array(Vec::new())),
        // illegal
        Some(IllegalToken) => Err(ParseError::IllegalToken(tokens.next().unwrap())),
        // なにかしらのトークン
        Some(_) => {
            let mut arr_values: Vec<JSONValue> = Vec::new();
            loop {
                let v = parse_value(tokens)?;
                arr_values.push(v);
                match tokens.peek().map(|token| &token.kind) {
                    Some(Comma) => {
                        tokens.next().unwrap(); // `,` 刈り取り. continue
                    }
                    Some(RBracket) => {
                        tokens.next().unwrap(); // `]`刈り取り
                        return Ok(JSONValue::Array(arr_values));
                    }
                    None => return Err(ParseError::UnclosedParenOrBrace),
                    Some(_) => return Err(ParseError::UnexpectedToken(tokens.next().unwrap())),
                }
            }
        }
    }
}

fn parse_obj<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<JSONValue, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    use TokenKind::*;

    // 最初の `{` を刈り取り
    expect_peek(tokens, LBrace)?;

    match tokens.peek().map(|token| &token.kind) {
        // 閉じてない
        None => Err(ParseError::UnclosedParenOrBrace),
        // 空オブジェクト
        Some(RBrace) => Ok(JSONValue::Object(HashMap::new())),
        // illegal
        Some(IllegalToken) => Err(ParseError::IllegalToken(tokens.next().unwrap())),
        // なにかしらのトークン
        Some(_) => {
            let mut key_values = HashMap::new();
            loop {
                // keyの刈り取り
                let key = parse_objkey(tokens)?;
                // コロンの刈り取り
                expect_peek(tokens, Colon)?;
                // 値の刈り取り
                key_values.insert(key, parse_value(tokens)?);

                match tokens.peek().map(|token| &token.kind) {
                    Some(Comma) => {
                        tokens.next().unwrap();
                    }
                    Some(RBrace) => {
                        tokens.next().unwrap();
                        return Ok(JSONValue::Object(key_values));
                    }
                    Some(_) => return Err(ParseError::UnexpectedToken(tokens.next().unwrap())),
                    None => return Err(ParseError::UnclosedParenOrBrace),
                };
            }
        }
    }
}

fn parse_objkey<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<String, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    if let JSONValue::Str(key) = parse_string(tokens)? {
        Ok(key)
    } else {
        // string以外がくることはない
        unreachable!()
    }
}

// 次にくる予定のトークン種別を待ち受け
fn expect_peek<Tokens>(
    tokens: &mut Peekable<Tokens>,
    expected: TokenKind,
) -> Result<Token, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    if let Some(kind) = tokens.peek().map(|token| &token.kind) {
        if *kind == expected {
            Ok(tokens.next().unwrap())
        } else {
            Err(ParseError::UnexpectedToken(tokens.next().unwrap()))
        }
    } else {
        Err(ParseError::UnexpectedEOF)
    }
}

// 入力文字列からトークンが示す範囲の文字列を抽出する
fn extra_literal(token: &Token) -> String {
    token.literal.clone()
}

#[cfg(test)]
mod test {
    use super::super::lexer::Location;
    use super::super::lexer::TokenKind::*;
    use super::super::*;
    use super::*;

    #[test]
    fn test_parse_expected_token_ok() {
        let inputs = vec![r#""""#, "}", "]"];
        let expected = vec![StrBody, RBrace, RBracket];

        for (i, _) in inputs.iter().enumerate() {
            let tokens = lexer::lex(&inputs[i]);
            let mut tokens = tokens.into_iter().peekable();
            if let Ok(Token {
                kind,
                literal: _,
                loc: _,
            }) = expect_peek(&mut tokens, expected[i].clone())
            {
                assert_eq!(expected[i], kind);
            } else {
                panic!("");
            }
        }
    }
    #[test]
    fn test_parse_expected_token_ng1() {
        let input = "{";
        let expected = RBracket;

        let tokens = lexer::lex(input);
        let mut tokens = tokens.into_iter().peekable();
        assert_eq!(
            Err(ParseError::UnexpectedToken(Token {
                kind: LBrace,
                literal: String::from(input),
                loc: Location(0, 1)
            })),
            expect_peek(&mut tokens, expected.clone())
        );
    }
    #[test]
    fn parser_number() {
        let input = "12345";
        let value = parse(lexer::lex(input));
        assert_eq!(Ok(JSONValue::Num(12345 as f64)), value);
    }
    #[test]
    fn parser_number2() {
        let input = "-12345";
        let value = parse(lexer::lex(input));
        assert_eq!(Ok(JSONValue::Num(-12345 as f64)), value);
    }
    #[test]
    fn parser_number3() {
        let input = "0";
        let value = parse(lexer::lex(input));
        assert_eq!(Ok(JSONValue::Num(0.0)), value);
    }
    #[test]
    fn parser_number4() {
        let inputs = vec![
            ("0.0", 0.0),
            ("1", 1f64),
            ("1.2", 1.2),
            ("-0.3", -0.3),
            ("-1234.56", -1234.56),
        ];
        for t in inputs {
            let value = parse(lexer::lex(t.0));
            assert_eq!(Ok(JSONValue::Num(t.1)), value);
        }
    }
    #[test]
    fn parser_bool_false() {
        let input = "false";
        let value = parse(lexer::lex(input));
        assert_eq!(Ok(JSONValue::Bool(false)), value);
    }

    #[test]
    fn parser_null() {
        let input = " null ";
        let value = parse(lexer::lex(input));
        assert_eq!(Ok(JSONValue::Null), value);
    }

    #[test]
    fn parser_string() {
        let input = r#""helloWORLD""#;
        let value = parse(lexer::lex(input));
        assert_eq!(Ok(JSONValue::Str("helloWORLD".to_string())), value);
    }
    #[test]
    fn parser_bool_true() {
        let input = "true";
        let value = parse(lexer::lex(input));
        assert_eq!(Ok(JSONValue::Bool(true)), value);
    }

    #[test]
    fn parser_array1() {
        let input = r#" [null, 123,  "hello" ]"#;
        let arr = vec![
            JSONValue::Null,
            JSONValue::Num(123 as f64),
            JSONValue::Str("hello".to_string()),
        ];
        let value = parse(lexer::lex(input));
        assert_eq!(Ok(JSONValue::Array(arr)), value);
    }
    #[test]
    fn parser_array2() {
        let input = " [ ] ";
        let arr = vec![];
        let value = parse(lexer::lex(input));
        assert_eq!(Ok(JSONValue::Array(arr)), value);
    }
    #[test]
    fn parser_obj_num() {
        let input = r#"{"key": 1}"#;
        let obj = JSONValue::Object({
            let mut map = HashMap::new();
            map.insert("key".to_string(), JSONValue::Num(1 as f64));
            map
        });

        let value = parse(lexer::lex(input));
        assert_eq!(Ok(obj), value);
    }
    #[test]
    fn parser_obj_string() {
        let input = r#"{"key": "string"}"#;
        let obj = JSONValue::Object({
            let mut map = HashMap::new();
            map.insert("key".to_string(), JSONValue::Str("string".to_string()));
            map
        });

        let value = parse(lexer::lex(input));
        assert_eq!(Ok(obj), value);
    }
    #[test]
    fn parser_obj_bool() {
        let input = r#"{"key": true }"#;
        let obj = JSONValue::Object({
            let mut map = HashMap::new();
            map.insert("key".to_string(), JSONValue::Bool(true));
            map
        });

        let value = parse(lexer::lex(input));
        assert_eq!(Ok(obj), value);
    }
    #[test]
    fn parser_obj_array() {
        let input = r#"{"key": [null,2, "hello"] }"#;
        let arr = vec![
            JSONValue::Null,
            JSONValue::Num(2 as f64),
            JSONValue::Str("hello".to_string()),
        ];
        let obj = JSONValue::Object({
            let mut map = HashMap::new();
            map.insert("key".to_string(), JSONValue::Array(arr));
            map
        });

        let value = parse(lexer::lex(input));
        assert_eq!(Ok(obj), value);
    }
    #[test]
    fn parser_obj_empty() {
        let input = r#"{}"#;
        let obj = JSONValue::Object(HashMap::new());

        let value = parse(lexer::lex(input));
        assert_eq!(Ok(obj), value);
    }
    #[test]
    fn parser_array_obj1() {
        let input = r#"[ {"key": 1 }]"#;
        let obj = JSONValue::Object({
            let mut map = HashMap::new();
            map.insert("key".to_string(), JSONValue::Num(1f64));
            map
        });

        let arr = vec![obj];
        let value = parse(lexer::lex(input));
        assert_eq!(Ok(JSONValue::Array(arr)), value);
    }
    #[test]
    fn parser_array_obj2() {
        let input = r#"[ {"key": 1 },{"hoge": 2 }  ]"#;
        let obj = JSONValue::Object({
            let mut map = HashMap::new();
            map.insert("key".to_string(), JSONValue::Num(1f64));
            map
        });
        let obj2 = JSONValue::Object({
            let mut map = HashMap::new();
            map.insert("hoge".to_string(), JSONValue::Num(2f64));
            map
        });

        let arr = vec![obj, obj2];
        let value = parse(lexer::lex(input));
        assert_eq!(Ok(JSONValue::Array(arr)), value);
    }
    #[test]
    fn parser_array_obj3() {
        let input = r#"[ 1, {"key": 1 }  ]"#;
        let obj = JSONValue::Object({
            let mut map = HashMap::new();
            map.insert("key".to_string(), JSONValue::Num(1f64));
            map
        });
        let obj2 = JSONValue::Num(1f64);

        let arr = vec![obj2, obj];
        let value = parse(lexer::lex(input));
        assert_eq!(Ok(JSONValue::Array(arr)), value);
    }
    #[test]
    fn parser_my_test() {
        let input = r#"{
            "key":1.0
        }"#;
        let value = parse(lexer::lex(input));
        assert_eq!(
            Ok(JSONValue::Object(
                [("key".to_string(), JSONValue::Num(1f64))]
                    .iter()
                    .cloned()
                    .collect::<HashMap<String, JSONValue>>()
            )),
            value
        );
    }
}
