//! # Rustに慣れるためJSONパーサーを書いてみた
//!
//! * 仕様は[こちら](https://www.json.org/json-en.html)を参照した
//!
//! ## TODO
//!
//! * numberのexponent表記
//!
//! ## 感想
//!
//! * parseで値を作らずに, 評価機を別途作ってもよかった.
pub mod lexer;
pub mod parser;
pub mod printer;
