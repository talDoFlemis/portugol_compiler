use crate::token::*;
use anyhow::{bail, Result};
use thiserror::Error;

pub mod macros;

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Invalid header (expected {expected:?}, got {found:?})")]
    MissingExpectedToken { expected: Token, found: Token },

    #[error("Unknown Symbol found {symbol:?}")]
    UnknownSymbol { symbol: String },

    #[error("Unable to parse the number {number:?}")]
    UnknownNumber { number: String },

    #[error("Unable to parse the unknown operatr {operator:?}")]
    UnknownOperator { operator: String },

    #[error("Unable to determinate the end of string")]
    UnfinishedString,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    //Human Readable format
    pub cur_col: usize,
    pub cur_line: usize,
    //Raw format in bytes for AST
    pub code_offset: usize,

    pub chars: std::iter::Peekable<std::str::Chars<'a>>,
}

impl<'a> Lexer<'a> {
    //Generate a new lexer
    pub fn new(chars: &'a str) -> Lexer<'a> {
        Lexer {
            cur_col: 1,
            cur_line: 1,
            code_offset: 0,
            chars: chars.chars().peekable(),
        }
    }

    //Search for the next non whitespace token
    fn skip_whitespace(&mut self) {
        while self.chars.next_if(|c| c.is_whitespace()).is_some() {}
    }

    //Consume the next char if it exists and update the lexer position
    fn consume_char(&mut self) -> Option<char> {
        match self.chars.next() {
            Some(c) => {
                self.cur_col += 1;
                self.code_offset += 1;

                if c == '\n' {
                    self.cur_line += 1;
                    self.cur_col += 1;
                }

                Some(c)
            }
            None => None,
        }
    }

    fn parse_number(&mut self, start: char) -> Result<TokenType> {
        let mut seen_dot = false;
        //TODO: Impl expression resolvers
        // let mut seen_exp = false;
        let radix = 10;

        let mut number = String::from(start);

        if start == '.' {
            seen_dot = true;
        }

        loop {
            match self.consume_char() {
                Some(c) if c == '.' => {
                    number.push(c);
                    match seen_dot {
                        true => {
                            bail!(LexerError::UnknownNumber { number })
                        }
                        false => {
                            seen_dot = true;
                        }
                    }
                }
                Some(c) if c.is_digit(radix) => number.push(c),
                Some(c) if c.is_ascii_alphabetic() => {
                    number.push(c);
                    bail!(LexerError::UnknownNumber { number })
                }
                _ => {
                    break Ok(TokenType::Numeric {
                        raw: number,
                        hint: if seen_dot {
                            NumericHint::FloatingPoint
                        } else {
                            NumericHint::Integer
                        },
                    })
                }
            }
        }
    }

    //TODO: Add escaping
    fn parse_string(&mut self) -> Result<TokenType> {
        let mut buf = String::new();

        loop {
            match self.consume_char() {
                Some('"') => break Ok(TokenType::String(buf)),
                Some(c) => buf.push(c),
                None => bail!(LexerError::UnfinishedString),
            }
        }
    }

    fn handle_operator(&mut self, start: &char) -> Result<TokenType> {
        let mut buf = String::from(*start);

        if let Some(c) = self.consume_char() {
            if Operator::contains(&c) {
                buf.push(c);
            }
        };

        match Operator::parse_operator(&buf) {
            Some(op) => Ok(TokenType::Op(op)),
            None => bail!(LexerError::UnknownOperator { operator: buf }),
        }
    }

    fn handle_punctuation(&mut self, c: &char) -> Result<TokenType> {
        match Punctuation::parse_punctuation(c) {
            Some(punc) => Ok(TokenType::Punct(punc)),
            None => bail!(LexerError::UnknownSymbol {
                symbol: String::from(*c)
            }),
        }
    }

    fn handle_id(&mut self, start: &char) -> Result<TokenType> {
        let mut buf = String::from(*start);

        loop {
            match self.chars.peek() {
                Some(c) if c.is_alphanumeric() || *c == '_' => {
                    buf.push(self.consume_char().unwrap())
                }
                _ => break,
            }
        }

        if let Some(keyword) = Keyword::parse_keyword(&buf) {
            Ok(TokenType::Key(keyword))
        } else {
            Ok(TokenType::ID(buf))
        }
    }

    fn tokenize(&mut self, c: char) -> Result<TokenType> {
        match c {
            '0'..='9' | '.' => Ok(self.parse_number(c)?),
            '"' => Ok(self.parse_string()?),
            c if Operator::contains(&c) => Ok(self.handle_operator(&c)?),
            c if Punctuation::contains(&c) => Ok(self.handle_punctuation(&c)?),
            c if c.is_ascii_alphabetic() || c == '_' => Ok(self.handle_id(&c)?),
            _ => bail!(LexerError::UnknownSymbol {
                symbol: String::from(c)
            }),
        }
    }

    pub fn next_token(&mut self) -> Result<TokenType> {
        self.skip_whitespace();

        match self.consume_char() {
            Some(c) => self.tokenize(c),
            None => (Ok(TokenType::EOF)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ignore_whitespace() -> Result<()> {
        let mut lexer = Lexer::new("   {");
        assert_eq!(lexer.next_token()?, TokenType::Punct(Punctuation::LBrace));
        Ok(())
    }

    #[test]
    fn parsing_str() -> Result<()> {
        let mut lexer = Lexer::new("\"aoeuaoeu\"");
        assert_eq!(
            lexer.next_token()?,
            TokenType::String("aoeuaoeu".to_string())
        );

        Ok(())
    }

    #[test]
    fn parsing_identifiers() {
        let mut lexer = Lexer::new("aoeu");
        assert_eq!(
            lexer.next_token().unwrap(),
            TokenType::ID(String::from("aoeu"))
        );
    }

    #[test]
    fn next_token() {
        let input = r#"joazin_poggers_ntc_22 = 5"#;
        let mut lexer = Lexer::new(input);
        let tests = [
            TokenType::ID(String::from("joazin_poggers_ntc_22")),
            TokenType::Op(Operator::Assign),
            TokenType::Numeric {
                raw: String::from("5"),
                hint: NumericHint::Integer,
            },
        ];

        for (_, expected_token) in tests.iter().enumerate() {
            let token = lexer.next_token().unwrap();
            assert_eq!(token, *expected_token);
        }
    }
}
