use portugol_compiler::lexer::{Lexer, TokenType};

fn main() {
    let mut lexer = Lexer::new("{{}}");

    loop {
        match lexer.next_token() {
            Ok(TokenType::EOF) => break,
            Ok(t) => println!("{t:?}"),
            Err(t) => println!("{t:?}"),
        }
    }
}
