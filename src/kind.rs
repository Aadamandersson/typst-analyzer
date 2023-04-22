/// Represents all possible syntactic constructs for `Typst`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum SyntaxKind {
    /// An identifier.
    /// E.g., `foo`.
    Ident,
    /// An integer.
    /// E.g., `123`.
    Int,
    /// A float.
    /// E.g., `12.3`
    Float,
    /// A string.
    /// E.g., `"foo`"
    String,
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `=`
    Eq,
    /// `<`
    Lt,
    /// `>`
    Gt,
    /// `.`
    Dot,
    /// `+=`
    PlusEq,
    /// `-=`
    MinusEq,
    /// `*=`
    StarEq,
    /// `/=`
    SlashEq,
    /// `!=`
    Ne,
    /// `==`
    EqEq,
    /// `=>`
    Arrow,
    /// `<=`
    Le,
    /// `>=`
    Ge,
    /// `..`
    DotDot,
    /// `(`
    OpenParen,
    /// `[´
    OpenBrack,
    /// `{`
    OpenBrace,
    /// `)`
    CloseParen,
    /// `]`
    CloseBrack,
    /// `}`
    CloseBrace,
    /// `,`
    Comma,
    /// `;`
    Semi,
    /// `:`
    Colon,
    /// `$`
    Dollar,
    /// ```
    Backtick,
    /// `as` keyword.
    As,
    /// `auto` keyword
    Auto,
    /// `break` keyword.
    Break,
    /// `continue` keyword.
    Continue,
    /// `else` keyword.
    Else,
    /// `for` keyword.
    For,
    /// `if` keyword.
    If,
    /// `import` keyword.
    Import,
    /// `in` keyword.
    In,
    /// `include` keyword.
    Include,
    /// `let` keyword.
    Let,
    /// `none` keyword
    None,
    /// `return` keyword.
    Return,
    /// `set` keyword.
    Set,
    /// `show` keyword.
    Show,
    /// `while` keyword.
    While,
    /// E.g., (' ', '\t', '\n', etc...)
    Whitespace,
    /// Represents code in the source text.
    Code,
    /// E.g., `1`
    Literal,
    /// E.g., `1 + 2`
    BinaryExpr,
    /// Lex or parse error.
    Error,
    /// End of file.
    Eof,
}

impl SyntaxKind {
    /// Returns `true` if this `SyntaxKind` is trivia.
    pub fn is_trivia(self) -> bool {
        matches!(self, SyntaxKind::Whitespace)
    }
}
