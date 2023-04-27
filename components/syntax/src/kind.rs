/// Represents all possible syntactic constructs for `Typst`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
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
    /// `_`
    Underscore,
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
    /// `and` keyword.
    And,
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
    /// `false` keyword.
    False,
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
    /// `not` keyword.
    Not,
    /// `or` keyword.
    Or,
    /// `return` keyword.
    Return,
    /// `set` keyword.
    Set,
    /// `show` keyword.
    Show,
    /// `true` keyword.
    True,
    /// `while` keyword.
    While,
    /// `/* ... */` or `// ...`
    Comment,
    /// E.g., (' ', '\t', '\n', etc...)
    Whitespace,
    /// Represents code in the source text.
    Code,
    /// E.g., `1`
    Literal,
    /// E.g., `1 + 2`
    BinaryExpr,
    /// `let pat [= expr]`
    /// E.g., `let foo = 1`
    LetBinding,
    /// A function pattern.
    /// E.g., `add(x, y)` in `let add(x, y) = x + y`
    FnPat,
    /// An identifier pattern.
    /// E.g., `foo`
    IdentPat,
    /// A wildcard pattern.
    /// `_`
    WildcardPat,
    /// A parameter list.
    /// E.g., `(x, y)`
    Params,
    /// A parameter.
    /// E.g., `x`
    Param,
    /// E.g., `-1`
    UnaryExpr,
    /// Lex or parse error.
    Error,
    /// End of file.
    Eof,
}

impl SyntaxKind {
    /// Returns `true` if this `SyntaxKind` is trivia.
    pub fn is_trivia(self) -> bool {
        matches!(self, SyntaxKind::Comment | SyntaxKind::Whitespace)
    }
}

impl From<u16> for SyntaxKind {
    fn from(value: u16) -> Self {
        assert!(value <= SyntaxKind::Eof as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(value) }
    }
}

impl From<SyntaxKind> for u16 {
    fn from(kind: SyntaxKind) -> Self {
        kind as u16
    }
}
