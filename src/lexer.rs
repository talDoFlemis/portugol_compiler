use anyhow::{anyhow, Result};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Invalid header (expected {expected:?}, got {found:?})")]
    MissingExpectedToken { expected: TokenType, found: Token },

    #[error("Unknown Symbol found {symbol:?}")]
    UnknownSymbol { symbol: String },

    #[error("Misbalanced symbol {symbol:?}, expected {expected:?}")]
    MisbalancedSymbol { symbol: char, expected: char },
}

#[derive(Debug)]
pub enum PunctuationType {
    Open,
    Close,
}

#[derive(Debug)]
pub enum Operators {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Assign,
}

#[derive(Debug)]
pub enum Keyword {
    IF,
    THEN,
    RETURN,
}

#[derive(Debug)]
pub enum NumericHint {
    Interger,
    FloatingPoint,
}

pub type Token = TokenType;

#[derive(Debug)]
pub enum TokenType {
    //End of token string
    EOF,
    //Identifier
    ID,
    //Pontuation [,{.(
    Punctuation { raw: char, r#type: PunctuationType },
    //Integer Literal
    Numeric { raw: String, hint: NumericHint },
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
        }
    }

    fn push_symbol(&mut self, c: &char) -> i32 {
        match self.balancing_shit.get_mut(&c) {
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

    fn pop_symbol(&mut self, c: &char) -> Result<i32> {
        match self.balancing_shit.get_mut(&Lexer::map_balance(&c)) {
            Some(v) => {
                if *v >= 1 {
                    *v -= 1;
                    Ok(*v)
                } else {
                    Err(anyhow!(LexerError::MisbalancedSymbol {
                        symbol: *c,
                        expected: Lexer::map_balance(&c)
                    }))
                }
            }
            None => Err(anyhow!(LexerError::MisbalancedSymbol {
                symbol: *c,
                expected: Lexer::map_balance(&c)
            })),
        }
    }

    //Search for the next non whitespace token
    fn skip_whitespace(&mut self) {
        while let Some(c) = self.chars.next_if(|c| c.is_whitespace()) {}
    }

    fn consume_char(&mut self) -> Option<char> {
        match self.chars.next() {
            Some(c) => {
                self.cur_col += 1;
                self.code_offset += 1;
                if c == '\n' {
                    self.cur_line += 1;
                    self.cur_col = 1;
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
