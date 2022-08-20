use anyhow::{bail, Result};
use thiserror::Error;

pub mod macros;

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Invalid header (expected {expected:?}, got {found:?})")]
    MissingExpectedToken { expected: Token, found: Token },

    #[error("Unknown Symbol found {symbol:?}")]
    UnknownSymbol { symbol: String },

    #[error("Misbalanced symbol {symbol:?}, expected {expected:?}")]
    MisbalancedSymbol { symbol: char, expected: char },

    #[error("Unable to parse the number {number:?}")]
    UnknownNumber { number: String },

    #[error("Unable to determinate the end of string")]
    UnfinishedString,
}

pub type Token = TokenType;
pub type BalancingDepth = i32;

#[derive(Debug, PartialEq, Eq)]
pub enum PunctuationKind {
    Open(BalancingDepth),
    Close(BalancingDepth),
    Separator,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Operators {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Assign,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Keyword {
    IF,
    THEN,
    RETURN,
}

#[derive(Debug, PartialEq, Eq)]
pub enum NumericHint {
    Integer,
    FloatingPoint,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenType {
    //End of token string
    EOF,
    //Identifier
    ID,
    //Pontuation [ { ( , .
    Punctuation { raw: char, kind: PunctuationKind },
    //Integer Literal
    Numeric { raw: String, hint: NumericHint },
    String(String),
    //Operators
    Op(Operators),
    //Keywords
    Key(Keyword),
    //Other characters
    Other(char),
}

#[derive(Debug)]
pub struct Lexer<'a> {
    //Human Readable format
    pub cur_col: usize,
    pub cur_line: usize,
    //Raw format in bytes for AST
    pub code_offset: usize,

    pub chars: std::iter::Peekable<std::str::Chars<'a>>,
    pub balancing_shit: std::collections::HashMap<char, i32>,
}

impl<'a> Lexer<'a> {
    //Generate a new lexer
    pub fn new(chars: &'a str) -> Lexer<'a> {
        Lexer {
            cur_col: 1,
            cur_line: 1,
            code_offset: 0,
            chars: chars.chars().peekable(),
            balancing_shit: std::collections::HashMap::new(),
        }
    }

    fn map_balance(c: &char) -> char {
        match c {
            ')' => '(',
            '}' => '{',
            ']' => '[',
            '(' => ')',
            '{' => '}',
            '[' => ']',
            _ => panic!("Can't balance something that isn't supposed to be balanced"),
        }
    }

    fn push_symbol(&mut self, c: &char) -> BalancingDepth {
        match self.balancing_shit.get_mut(c) {
            Some(d) => {
                *d += 1;
                *d - 1
            }
            None => {
                self.balancing_shit.insert(*c, 1);
                0
            }
        }
    }

    fn pop_symbol(&mut self, c: &char) -> Result<BalancingDepth> {
        match self.balancing_shit.get_mut(&Lexer::map_balance(c)) {
            Some(v) => {
                if *v >= 1 {
                    *v -= 1;
                    Ok(*v)
                } else {
                    bail!(LexerError::MisbalancedSymbol {
                        symbol: *c,
                        expected: Lexer::map_balance(c)
                    })
                }
            }
            None => bail!(LexerError::MisbalancedSymbol {
                symbol: *c,
                expected: Lexer::map_balance(c)
            }),
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

    // fn parse_number(&mut self, start: char) -> Result<TokenType> {
    //     let mut seen_dot = false;
    //     let mut seen_exp = false;
    //
    //     let mut number = String::from(start);
    //
    //     loop {
    //         match self.chars.peek() {
    //             Some(c) if *c == '.' && seen_dot => {}
    //         }
    //     }
    // }

    fn tokenize(c: char) -> Result<TokenType> {
        match c {
            '(' | '{' | '[' => Ok(TokenType::Punctuation { raw: &c, r#type:  }),
            _ => Err(anyhow!(LexerError::UnknownSymbol {
                symbol: String::from(c)
            })),
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
    fn ignore_whitespace() {
        let mut lexer = Lexer::new("   k");
        lexer.skip_whitespace();
    }
}
