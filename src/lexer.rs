use std::iter::Peekable;

/// トークン種別
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    /// [
    LBrace,
    /// ]
    RBrace,
    /// {
    LParen,
    /// }
    RParen,
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
    // 位置
    pub loc: Location,
}

impl Token {
    fn new(kind: TokenKind, start: usize, end: usize) -> Self {
        Token {
            kind,
            loc: Location(start, end),
        }
    }
}

/// 入力からTokenの配列と入力をcharのsliceに変換したものを返す.
/// * char単位だと簡単に複数バイト文字が扱えるのでcharのsliceベースで処理することにする.
/// * 位置情報がchar slice内の値となるので, 返す位置情報とリンクするcharのスライスを合わせて返す
pub fn lex(input: &str) -> (Vec<Token>, Vec<char>) {
    use TokenKind::*;
    let mut tokens = Vec::new();
    let mut peekable = input.chars().peekable();
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
        } else if *c == '"' {
            // peekは借用である
            let (token, after_pos) = lex_strbody(&mut peekable, pos);
            tokens.push(token);
            pos = after_pos;
        } else {
            match c {
                '[' => tokens.push(Token::new(LBrace, pos, pos + 1)),
                ']' => tokens.push(Token::new(RBrace, pos, pos + 1)),
                '{' => tokens.push(Token::new(LParen, pos, pos + 1)),
                '}' => tokens.push(Token::new(RParen, pos, pos + 1)),
                ',' => tokens.push(Token::new(Comma, pos, pos + 1)),
                '.' => tokens.push(Token::new(Dot, pos, pos + 1)),
                '+' => tokens.push(Token::new(Plus, pos, pos + 1)),
                '-' => tokens.push(Token::new(Minus, pos, pos + 1)),
                ':' => tokens.push(Token::new(Colon, pos, pos + 1)),
                ' ' | '\n' | '\r' | '\t' => { /* white spece 読み飛ばし*/ }
                _ => tokens.push(Token::new(IllegalToken, pos, pos + 1)),
            };
            peekable.next().unwrap();
            pos += 1;
        }
    }
    (tokens, input.chars().collect::<Vec<char>>())
}

// 数列
// 0, 1, 12, 01, ...
fn lex_digits<Tokens>(input: &mut Peekable<Tokens>, start: usize) -> (Token, usize)
where
    Tokens: Iterator<Item = char>,
{
    let mut pos = start;
    while let Some(c) = input.peek() {
        if c.is_digit(10) {
            input.next().unwrap();
            pos += 1;
        } else {
            break;
        }
    }
    (Token::new(TokenKind::Digits, start, pos), pos)
}

// アルファベットの列
// a, A, aB, ABC
fn lex_chars<Tokens>(input: &mut Peekable<Tokens>, start: usize) -> (Token, usize)
where
    Tokens: Iterator<Item = char>,
{
    let mut pos = start;
    while let Some(c) = input.peek() {
        if c.is_alphabetic() {
            input.next().unwrap();
            pos += 1;
        } else {
            break;
        }
    }
    (Token::new(TokenKind::CharSeq, start, pos), pos)
}

// `"`に囲まれた範囲. 間に空白文字も含む
// illegallな場合は, 先頭の`"`を不正なトークンの始まり位置と考える.
fn lex_strbody<Tokens>(input: &mut Peekable<Tokens>, start: usize) -> (Token, usize)
where
    Tokens: Iterator<Item = char>,
{
    use TokenKind::*;

    let mut pos = start;

    // 先頭の`"`を刈り取り
    input.next().unwrap();
    pos += 1;

    if let None = input.peek() {
        (Token::new(TokenKind::IllegalToken, start, pos), pos)
    } else if let Some('"') = input.peek() {
        // 空文字列
        input.next().unwrap();
        pos += 1;
        (Token::new(TokenKind::StrBody, start, pos), pos)
    } else {
        loop {
            match input.next() {
                // 終了
                Some('"') => return (Token::new(StrBody, start, pos + 1), pos + 1),
                // 何らかのchar
                Some(c) => {
                    pos += 1;
                    if c.is_ascii_control() {
                        return (Token::new(IllegalToken, start, pos), pos);
                    } else if c == '\\' {
                        match lex_after_bs(input, pos) {
                            Ok(pos_new) => pos = pos_new,
                            Err(pos_new) => {
                                return (Token::new(IllegalToken, start, pos_new), pos_new);
                            }
                        }
                    } else {
                        // その他の受け入れ可能なchar
                    }
                }
                // 閉じられていない
                None => return (Token::new(IllegalToken, start, pos), pos),
            }
        }
    }
}

// `\`以降の文字
fn lex_after_bs<Tokens>(input: &mut Peekable<Tokens>, start: usize) -> Result<usize, usize>
where
    Tokens: Iterator<Item = char>,
{
    let mut pos = start;

    match input.next() {
        //  `\`のあとが切れている.
        None => Err(pos),
        Some(escape) => {
            pos = pos + 1;
            match escape {
                // パース継続
                '\"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't' => Ok(pos),
                //`\u1234`
                'u' => {
                    let hex_4 = input.take(4).collect::<Vec<char>>();
                    pos = pos + hex_4.len();
                    if hex_4.len() != 4 {
                        Err(pos)
                    } else if hex_4.iter().all(|h| h.is_ascii_hexdigit()) == false {
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
    let token = Token::new(TokenKind::Digits, 1, 4);
    let expect = vec![token];
    let input = " 123 ";
    assert_eq!(expect, lex(&input).0);
}
#[test]
fn lex_digit2() {
    let token = Token::new(TokenKind::Digits, 0, 1);
    let expect = vec![token];
    let input = "1  ";
    assert_eq!(expect, lex(&input).0);
}

#[test]
fn lex_digit_and_char1() {
    let token = Token::new(TokenKind::Digits, 0, 1);
    let mut expect = vec![token];
    expect.push(Token::new(TokenKind::CharSeq, 3, 6));
    let input = "1  abc";
    assert_eq!(expect, lex(&input).0);
}
#[test]
fn lex_digit_and_char2() {
    let token = Token::new(TokenKind::CharSeq, 0, 3);
    let mut expect = vec![token];
    expect.push(Token::new(TokenKind::Digits, 3, 4));
    let input = "abc2";
    assert_eq!(expect, lex(&input).0);
}
#[test]
fn lex_symbols() {
    use TokenKind::*;
    let input = r#"{}[]"#;
    let expect = vec![
        Token::new(LParen, 0, 1),
        Token::new(RParen, 1, 2),
        Token::new(LBrace, 2, 3),
        Token::new(RBrace, 3, 4),
    ];
    assert_eq!(expect, lex(input).0);
}

#[test]
fn lex_strbody_ok() {
    use TokenKind::*;
    let inputs = vec![
        //
        r#""""#, //
        // 012
        r#""key""#, //
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
        ///
        Token::new(StrBody, 0, 2), //
        Token::new(StrBody, 0, 5),  //
        Token::new(StrBody, 0, 3),  //
        Token::new(StrBody, 0, 15), //
        Token::new(StrBody, 0, 14), //
        Token::new(StrBody, 0, 14), //
        Token::new(StrBody, 0, 14), //
        Token::new(StrBody, 0, 14), //
    ];

    for (i, input) in inputs.iter().enumerate() {
        assert_eq!(expect[i], lex_strbody(&mut input.chars().peekable(), 0).0);
    }
}
#[test]
fn lex_strbody_ng() {
    use TokenKind::*;
    let inputs = vec![
        // 終端の"がない
        //   空白
        r#"""#, //
        //   1文字
        r#""a"#, //
        //   bsのあと
        r#""\"#, //
        //   エスケープ文字のあと
        r#""\""#, //
        r#""\\"#, //
        r#""\n"#, //
        // ユニコードのあと
        r#""\u1234"#, //
        // 許可してないエスケープ
        r#""\a""#, //
        // ユニコード不足
        r#""\u123 ""#, //
        // ユニコード異常
        r#""\u123g""#, //
    ];
    let expect = vec![
        ///
        Token::new(IllegalToken, 0, 1), //
        Token::new(IllegalToken, 0, 2), //
        Token::new(IllegalToken, 0, 2), //
        Token::new(IllegalToken, 0, 3), //
        Token::new(IllegalToken, 0, 3), //
        Token::new(IllegalToken, 0, 3), //
        Token::new(IllegalToken, 0, 7), //
        Token::new(IllegalToken, 0, 3), //
        Token::new(IllegalToken, 0, 7), //
        Token::new(IllegalToken, 0, 7), //
    ];

    for (i, input) in inputs.iter().enumerate() {
        assert_eq!(expect[i], lex_strbody(&mut input.chars().peekable(), 0).0);
    }
}

#[test]
fn lex_strbody_check() {
    use TokenKind::*;
    let input = r#""ab\u1234""#;
    let expect = Token::new(StrBody, 0, 10);
    let tokens = lex(input);
    println!("{:?}", tokens);

    assert_eq!(expect, lex_strbody(&mut input.chars().peekable(), 0).0);
}
