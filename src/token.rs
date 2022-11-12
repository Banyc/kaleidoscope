/// - Learn more: <https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl01.html#the-lexer>
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    EOF,

    // commands
    Def,
    Extern,

    // primary
    Identifier(String),
    Number(f64),

    // control
    If,
    Then,
    Else,
    For,
    In,

    Unknown(char),
}
