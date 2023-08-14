use tracing::{error, Level};
use ucc::{lex, parser::Parser};

mod ucc;

fn main() {
    let collector = tracing_subscriber::fmt()
        .with_max_level(Level::TRACE)
        .finish();
    tracing::subscriber::set_global_default(collector).unwrap();
    // let code = r#"const int * x(int*(*)(int, double))"#;
    let code = r#"int"#;
    let mut lexer = lex::Lexer::new(&code);
    let tokens = lexer.lex().unwrap_or_else(|e| {
        error!("{:?}", e);
        std::process::exit(1);
    });
    let mut parser = Parser::new(tokens);
    parser.parse().unwrap_or_else(|e| {
        error!("{:?}", e);
        std::process::exit(1);
    });
}
