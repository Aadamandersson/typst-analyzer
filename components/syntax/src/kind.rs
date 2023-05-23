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
    /// `#`
    Pound,
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
    /// `{ ... }`
    CodeBlock,
    /// `[ ... ]`
    ContentBlock,
    /// E.g., `1`
    Literal,
    /// E.g., `1 + 2`
    BinaryExpr,
    /// E.g., `-1`
    UnaryExpr,
    /// E.g., `(1 + 2)`
    ParenExpr,
    /// E.g., `while condition { ... }`
    WhileExpr,
    /// E.g., `for n in "123" { ... }`
    ForExpr,
    /// E.g., `if cond { ... } else if cond { ... } else { ... }`
    IfExpr,
    /// `break`
    BreakExpr,
    /// `continue`
    ContinueExpr,
    /// E.g., `(1, 2, 3)`
    ArrayExpr,
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
    /// E.g., a parameter name.
    Name,
    /// A reference to a name.
    /// E.g., `x`
    NameRef,
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

#[macro_export]
macro_rules! Token {
    [ident] => { $crate::kind::SyntaxKind::Ident };
    [int] => { $crate::kind::SyntaxKind::Int };
    [float] => { $crate::kind::SyntaxKind::Float };
    [string] => { $crate::kind::SyntaxKind::String };
    [+] => { $crate::kind::SyntaxKind::Plus };
    [-] => { $crate::kind::SyntaxKind::Minus };
    [*] => { $crate::kind::SyntaxKind::Star };
    [/] => { $crate::kind::SyntaxKind::Slash };
    [=] => { $crate::kind::SyntaxKind::Eq };
    [<] => { $crate::kind::SyntaxKind::Lt };
    [>] => { $crate::kind::SyntaxKind::Gt };
    [.] => { $crate::kind::SyntaxKind::Dot };
    [+=] => { $crate::kind::SyntaxKind::PlusEq };
    [-=] => { $crate::kind::SyntaxKind::MinusEq };
    [*=] => { $crate::kind::SyntaxKind::StarEq };
    [/=] => { $crate::kind::SyntaxKind::SlashEq };
    [!=] => { $crate::kind::SyntaxKind::Ne };
    [==] => { $crate::kind::SyntaxKind::EqEq };
    [=>] => { $crate::kind::SyntaxKind::Arrow };
    [<=] => { $crate::kind::SyntaxKind::Le };
    [>=] => { $crate::kind::SyntaxKind::Ge };
    [..] => { $crate::kind::SyntaxKind::DotDot };
    [_] => { $crate::kind::SyntaxKind::Underscore };
    ['('] => { $crate::kind::SyntaxKind::OpenParen };
    ['['] => { $crate::kind::SyntaxKind::OpenBrack };
    ['{'] => { $crate::kind::SyntaxKind::OpenBrace };
    [')'] => { $crate::kind::SyntaxKind::CloseParen };
    [']'] => { $crate::kind::SyntaxKind::CloseBrack };
    ['}'] => { $crate::kind::SyntaxKind::CloseBrace };
    [,] => { $crate::kind::SyntaxKind::Comma };
    [:] => { $crate::kind::SyntaxKind::Semi };
    [;] => { $crate::kind::SyntaxKind::Colon };
    [$] => { $crate::kind::SyntaxKind::Dollar };
    [#] => { $crate::kind::SyntaxKind::Pound };
    ['`'] => { $crate::kind::SyntaxKind::Backtick };
    [and] => { $crate::kind::SyntaxKind::And };
    [as] => { $crate::kind::SyntaxKind::As };
    [auto] => { $crate::kind::SyntaxKind::Auto };
    [break] => { $crate::kind::SyntaxKind::Break };
    [continue] => { $crate::kind::SyntaxKind::Continue };
    [else] => { $crate::kind::SyntaxKind::Else };
    [false] => { $crate::kind::SyntaxKind::False };
    [for] => { $crate::kind::SyntaxKind::For };
    [if] => { $crate::kind::SyntaxKind::If };
    [import] => { $crate::kind::SyntaxKind::Import };
    [in] => { $crate::kind::SyntaxKind::In };
    [include] => { $crate::kind::SyntaxKind::Include };
    [let] => { $crate::kind::SyntaxKind::Let };
    [none] => { $crate::kind::SyntaxKind::None };
    [not] => { $crate::kind::SyntaxKind::Not };
    [or] => { $crate::kind::SyntaxKind::Or };
    [return] => { $crate::kind::SyntaxKind::Return };
    [set] => { $crate::kind::SyntaxKind::Set };
    [show] => { $crate::kind::SyntaxKind::Show };
    [true] => { $crate::kind::SyntaxKind::True };
    [while] => { $crate::kind::SyntaxKind::While };
    [comment] => { $crate::kind::SyntaxKind::Comment };
    [whitespace] => { $crate::kind::SyntaxKind::Whitespace };
    [eof] => { $crate::kind::SyntaxKind::Eof };
}
