#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    Identifier(String),
    PunctuationSoup(String),
    NumericLiteral(String),
    EqualSign,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Semicolons(usize),
    Space,
    Newline,
    Indent,
    Outdent,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Token::*;
        match self {
            Space => write!(f, "␠"),
            Newline => write!(f, "␤"),
            Indent => write!(f, "→"),
            Outdent => write!(f, "←"),
            EqualSign => write!(f, "="),
            OpenParen => write!(f, "("),
            CloseParen => write!(f, ")"),
            OpenBracket => write!(f, "["),
            CloseBracket => write!(f, "]"),
            Semicolons(count) => write!(f, "{}", ";".repeat(*count)),
            Identifier(s) | PunctuationSoup(s) | NumericLiteral(s) => write!(f, "{}", s),
        }
    }
}
