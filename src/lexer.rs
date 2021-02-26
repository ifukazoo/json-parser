use std::iter::Peekable;

/// トークン種別
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    /// [
    LBracket,
    /// ]
    RBracket,
    /// {
    LBrace,
    /// }
    RBrace,
    /// ,
    Comma,
    /// :
    Colon,
    /// .
    Dot,
    /// \+
    Plus,
    /// \-
    Minus,
    /// 0, 1, 012, 123, ...
    Digits,
    /// true, false, null, a, abc, ...
    CharSeq,
    /// "key"
    StrBody,
    /// 不正なトークン
    IllegalToken,
}

/// トークン位置情報
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Location(
    /// 開始位置
    pub usize,
    /// 終了位置+1
    pub usize,
);

/// トークン
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    // 種別
    pub kind: TokenKind,
    // リテラル
    pub literal: String,
    // 位置
    pub loc: Location,
}

impl Token {
    fn new(kind: TokenKind, literal: String, start: usize, end: usize) -> Self {
        Self {
            kind,
            literal,
            loc: Location(start, end),
        }
    }
}

pub type LexResult = Vec<Token>;

/// 入力からTokenの配列と入力をcharのsliceに変換したものを返す.
/// * char単位だと簡単に複数バイト文字が扱えるのでcharのsliceベースで処理することにする.
pub fn lex(input: &str) -> LexResult {
    use TokenKind::*;
    let mut tokens = Vec::new();
    let chars = input.chars().collect::<Vec<char>>();
    let mut peekable = chars.iter().peekable();
    let mut pos = 0;
    while let Some(c) = peekable.peek() {
        if c.is_digit(10) {
            let (token, after_pos) = lex_digits(&mut peekable, pos);
            tokens.push(token);
            pos = after_pos;
        } else if c.is_alphabetic() {
            let (token, after_pos) = lex_chars(&mut peekable, pos);
            tokens.push(token);
            pos = after_pos;
        } else if *c == &'"' {
            // cは参照の参照なので
            let (token, after_pos) = lex_strbody(&mut peekable, pos);
            tokens.push(token);
            pos = after_pos;
        } else {
            let literal = c.to_string();
            match c {
                '[' => tokens.push(Token::new(LBracket, literal, pos, pos + 1)),
                ']' => tokens.push(Token::new(RBracket, literal, pos, pos + 1)),
                '{' => tokens.push(Token::new(LBrace, literal, pos, pos + 1)),
                '}' => tokens.push(Token::new(RBrace, literal, pos, pos + 1)),
                ',' => tokens.push(Token::new(Comma, literal, pos, pos + 1)),
                '.' => tokens.push(Token::new(Dot, literal, pos, pos + 1)),
                '+' => tokens.push(Token::new(Plus, literal, pos, pos + 1)),
                '-' => tokens.push(Token::new(Minus, literal, pos, pos + 1)),
                ':' => tokens.push(Token::new(Colon, literal, pos, pos + 1)),
                ' ' | '\n' | '\r' | '\t' => { /* white spece 読み飛ばし*/ }
                _ => tokens.push(Token::new(IllegalToken, literal, pos, pos + 1)),
            };
            peekable.next().unwrap();
            pos += 1;
        }
    }
    tokens
}

// 数列
// 0, 1, 12, 01, ...
fn lex_digits<'a, Tokens>(input: &mut Peekable<Tokens>, start: usize) -> (Token, usize)
where
    Tokens: Iterator<Item = &'a char>,
{
    let mut s = String::new();
    let mut pos = start;
    while let Some(c) = input.peek() {
        if c.is_digit(10) {
            s.push(**c);
            input.next().unwrap();
            pos += 1;
        } else {
            break;
        }
    }
    (Token::new(TokenKind::Digits, s, start, pos), pos)
}

// アルファベットの列
// a, A, aB, ABC
fn lex_chars<'a, Tokens>(input: &mut Peekable<Tokens>, start: usize) -> (Token, usize)
where
    Tokens: Iterator<Item = &'a char>,
{
    let mut s = String::new();
    let mut pos = start;
    while let Some(c) = input.peek() {
        if c.is_alphabetic() {
            s.push(**c);
            input.next().unwrap();
            pos += 1;
        } else {
            break;
        }
    }
    (Token::new(TokenKind::CharSeq, s, start, pos), pos)
}

// `"`に囲まれた範囲. 間に空白文字も含む
// illegallな場合は, 先頭の`"`を不正なトークンの始まり位置と考える.
fn lex_strbody<'a, Tokens>(input: &mut Peekable<Tokens>, start: usize) -> (Token, usize)
where
    Tokens: Iterator<Item = &'a char>,
{
    use TokenKind::*;

    let mut literal = String::new();
    let mut pos = start;

    // 先頭の`"`を刈り取り
    let c = input.next().unwrap();
    literal.push(*c);
    pos += 1;

    if let None = input.peek() {
        (Token::new(IllegalToken, literal, start, pos), pos)
    } else if let Some('"') = input.peek() {
        // 空文字列
        let c = input.next().unwrap();
        literal.push(*c);
        pos += 1;
        (Token::new(StrBody, literal, start, pos), pos)
    } else {
        loop {
            match input.next() {
                // 終了
                Some('"') => {
                    literal.push('"');
                    pos += 1;
                    return (Token::new(StrBody, literal, start, pos), pos);
                }
                // 何らかのchar
                Some(c) => {
                    literal.push(*c);
                    pos += 1;
                    if c.is_ascii_control() {
                        return (Token::new(IllegalToken, literal, start, pos), pos);
                    } else if *c == '\\' {
                        match lex_after_bs(input, pos, &mut literal) {
                            Ok(pos_new) => {
                                pos = pos_new;
                            }
                            Err(pos_new) => {
                                return (
                                    Token::new(IllegalToken, literal, start, pos_new),
                                    pos_new,
                                );
                            }
                        }
                    } else {
                        // その他の受け入れ可能なchar
                    }
                }
                // 閉じられていない
                None => return (Token::new(IllegalToken, literal, start, pos), pos),
            }
        }
    }
}

// `\`以降の文字
fn lex_after_bs<'a, Tokens>(
    input: &mut Peekable<Tokens>,
    start: usize,
    literal: &mut String,
) -> Result<usize, usize>
where
    Tokens: Iterator<Item = &'a char>,
{
    let mut pos = start;
    match input.next() {
        //  `\`のあとが切れている.
        None => Err(pos),
        Some(escape) => {
            literal.push(*escape);
            pos = pos + 1;
            match escape {
                // パース継続
                '\"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't' => Ok(pos),
                //`\u1234`
                'u' => {
                    let hex_4 = input.take(4).map(|&c| c).collect::<String>();
                    literal.push_str(&hex_4);
                    pos = pos + hex_4.len();
                    if hex_4.len() != 4 {
                        Err(pos)
                    } else if hex_4.chars().all(|h| h.is_ascii_hexdigit()) == false {
                        Err(pos)
                    } else {
                        Ok(pos)
                    }
                }
                // `\`のあとが不正なchar
                _ => Err(pos),
            }
        }
    }
}

#[test]
fn lex_digit1() {
    let input = " 123 ";
    let token = Token::new(TokenKind::Digits, String::from(input.trim()), 1, 4);
    let expect = vec![token];
    assert_eq!(expect, lex(&input));
}
#[test]
fn lex_digit2() {
    let input = "1  ";
    let token = Token::new(TokenKind::Digits, String::from(input.trim()), 0, 1);
    let expect = vec![token];
    assert_eq!(expect, lex(&input));
}

#[test]
fn lex_digit_and_char1() {
    let input = "1  abc";
    let expect = vec![
        Token::new(TokenKind::Digits, "1".to_string(), 0, 1),
        Token::new(TokenKind::CharSeq, "abc".to_string(), 3, 6),
    ];
    assert_eq!(expect, lex(&input));
}
#[test]
fn lex_digit_and_char2() {
    let input = "abc2";
    let expect = vec![
        Token::new(TokenKind::CharSeq, "abc".to_string(), 0, 3),
        Token::new(TokenKind::Digits, "2".to_string(), 3, 4),
    ];
    assert_eq!(expect, lex(&input));
}
#[test]
fn lex_symbols() {
    use TokenKind::*;
    let input = r#"{}[]"#;
    let expect = vec![
        Token::new(LBrace, "{".to_string(), 0, 1),
        Token::new(RBrace, "}".to_string(), 1, 2),
        Token::new(LBracket, String::from("["), 2, 3),
        Token::new(RBracket, String::from("]"), 3, 4),
    ];
    assert_eq!(expect, lex(input));
}

#[test]
fn lex_strbody_ok() {
    use TokenKind::*;
    let inputs = vec![
        // 012
        r#""key""#, //
        //
        r#""""#, //
        // 012345
        r#"" ""#,
        // 0123
        r#"" key with sp ""#,
        // 0123          EF
        r#"" key w bs\\ ""#,
        // 0123          E
        r#"" key w dq\" ""#,
        // 0123          E
        r#"" key w dq\n ""#,
        // 0123          E
        r#"" u \u01aFz01""#,
        // 0123          E
    ];
    let expect = vec![
        Token::new(StrBody, String::from(inputs[0].trim()), 0, 5), //
        Token::new(StrBody, String::from(inputs[1].trim()), 0, 2), //
        Token::new(StrBody, String::from(inputs[2].trim()), 0, 3), //
        Token::new(StrBody, String::from(inputs[3].trim()), 0, 15), //
        Token::new(StrBody, String::from(inputs[4].trim()), 0, 14), //
        Token::new(StrBody, String::from(inputs[5].trim()), 0, 14), //
        Token::new(StrBody, String::from(inputs[6].trim()), 0, 14), //
        Token::new(StrBody, String::from(inputs[7].trim()), 0, 14), //
    ];

    for (i, input) in inputs.iter().enumerate() {
        let input = input.chars().collect::<Vec<char>>();
        let mut input = input.iter().peekable();
        assert_eq!(expect[i], lex_strbody(&mut input, 0).0);
    }
}

#[test]
fn lex_strbody_check() {
    use TokenKind::*;
    let input = r#""ab\u1234""#;
    let expect = Token::new(StrBody, String::from(input), 0, 10);
    let input = input.chars().collect::<Vec<char>>();
    let mut input = input.iter().peekable();
    assert_eq!(expect, lex_strbody(&mut input, 0).0);
}

#[test]
fn lex_strbody_ng() {
    use TokenKind::*;
    let inputs = vec![
        // 終端の"が不足
        //   空白
        r#"""#, //   "
        //   1文字
        r#""a"#, //  "a
        //   bsのあと
        r#""\"#, //  "\
        //   エスケープ文字のあと
        r#""\""#, // "\"
        //
        r#""\\"#, // "\\
        //
        r#""\n"#, // "\n
        //   ユニコードのあと
        r#""\u1234"#, //
        //
        // その他の不正
        //   許可してないエスケープ
        r#""\a""#, //
        //   コード不足
        r#""\u123 ""#, //
        //   ユニコード異常
        r#""\u123g""#, //
    ];
    let expect = vec![
        Token::new(IllegalToken, String::from(inputs[0]), 0, 1), //
        Token::new(IllegalToken, String::from(inputs[1]), 0, 2), //
        Token::new(IllegalToken, String::from(inputs[2]), 0, 2), //
        Token::new(IllegalToken, String::from(inputs[3]), 0, 3), //
        Token::new(IllegalToken, String::from(inputs[4]), 0, 3), //
        Token::new(IllegalToken, String::from(inputs[5]), 0, 3), //
        Token::new(IllegalToken, String::from(inputs[6]), 0, 7), //
        Token::new(IllegalToken, String::from("\"\\a"), 0, 3),   //
        Token::new(IllegalToken, String::from("\"\\u123 "), 0, 7), //
        Token::new(IllegalToken, String::from("\"\\u123g"), 0, 7), //
    ];

    for (i, input) in inputs.iter().enumerate() {
        let input = input.chars().collect::<Vec<char>>();
        let mut input = input.iter().peekable();
        assert_eq!(expect[i], lex_strbody(&mut input, 0).0);
    }
}
