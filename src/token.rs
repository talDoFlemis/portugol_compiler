use phf::phf_map;

pub type Token = TokenType;
pub type BalancingDepth = i32;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Punctuation {
    Colon,
    Semicolon,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Comma,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Less,
    LessEq,
    Eq,
    NotEq,
    LAnd,
    LOr,
    LNot,
    Assign,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Keyword {
    BEGIN,
    END,
    FUNCTION,
    PROCEDURE,
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
    ID(String),
    //Pontuation [ { ( , .
    Punct(Punctuation),
    //Integer Literal
    Numeric { raw: String, hint: NumericHint },
    //String Literal
    String(String),
    //Operators
    Op(Operator),
    //Keywords
    Key(Keyword),
    //Other characters
    Other(char),
}

impl Punctuation {
    pub fn contains(c: &char) -> bool {
        ":;,[{()}]".contains(*c)
    }

    pub fn parse_punctuation(c: &char) -> Option<Self> {
        match c {
            ';' => Some(Punctuation::Semicolon),
            ':' => Some(Punctuation::Colon),
            ',' => Some(Punctuation::Comma),
            '[' => Some(Punctuation::LBracket),
            '{' => Some(Punctuation::LBrace),
            ')' => Some(Punctuation::RParen),
            '}' => Some(Punctuation::RBrace),
            ']' => Some(Punctuation::RBracket),
            _ => None,
        }
    }
}

impl Operator {
    pub fn parse_operator(s: &str) -> Option<Self> {
        static OPERATORS: phf::Map<&'static str, Operator> = phf_map! {
            "+" => Operator::Add,
            "-" => Operator::Sub,
            "*" => Operator::Mul,
            "/" => Operator::Div,
            "%" => Operator::Mod,
            "<" => Operator::Less,
            "<=" => Operator::LessEq,
            "==" => Operator::Eq,
            "!=" => Operator::NotEq,
            "&&" => Operator::LAnd,
            "||" => Operator::LOr,
            "!" => Operator::LNot,
            "=" => Operator::Assign,
        };

        OPERATORS.get(s).cloned()
    }

    pub fn contains(c: &char) -> bool {
        "+-*/%<=!&|:".contains(*c)
    }
}

impl Keyword {
    pub fn parse_keyword(s: &str) -> Option<Self> {
        static KEYWORDS: phf::Map<&'static str, Keyword> = phf_map! {
            "inicio" => Keyword::BEGIN,
            "início" => Keyword::BEGIN,
            "fim" => Keyword::END,
            "se" => Keyword::IF,
            "entao" => Keyword::THEN,
            "então" => Keyword::THEN,
            "retorne" => Keyword::RETURN,
        };

        KEYWORDS.get(s).cloned()
    }
}
