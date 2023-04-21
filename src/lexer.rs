#![allow(dead_code)]
use std::{iter::Peekable, str::Chars};
use unicode_xid::UnicodeXID;

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
    /// An unknown character to the lexer.
    Unknown,
    /// End of file.
    Eof,
}

const EOF_CHAR: char = '\0';

/// Takes the source text and splits it into tokens.
pub(crate) struct Lexer<'s> {
    /// The characters in the source text.
    chars: Peekable<Chars<'s>>,
    /// The current position in the source text.
    pos: usize,
}

impl<'s> Lexer<'s> {
    /// Constructs a new `Lexer` with the given source text.
    pub(crate) fn new(src: &'s str) -> Self {
        let chars = src.chars().peekable();
        Self { chars, pos: 0 }
    }

    /// Returns the next `SyntaxKind` and its length.
    pub(crate) fn next(&mut self) -> (u32, SyntaxKind) {
        let first = self.peek();
        let start = self.pos;
        self.bump();
        let kind = match first {
            '0'..='9' => self.lex_numeric(),
            '+' => {
                if self.eat('=') {
                    SyntaxKind::PlusEq
                } else {
                    SyntaxKind::Plus
                }
            }
            '-' => {
                if self.eat('=') {
                    SyntaxKind::MinusEq
                } else {
                    SyntaxKind::Minus
                }
            }
            '*' => {
                if self.eat('=') {
                    SyntaxKind::StarEq
                } else {
                    SyntaxKind::Star
                }
            }
            '/' => {
                if self.eat('=') {
                    SyntaxKind::SlashEq
                } else {
                    SyntaxKind::Slash
                }
            }
            '!' if self.eat('=') => SyntaxKind::Ne,
            '=' => {
                if self.eat('=') {
                    SyntaxKind::EqEq
                } else if self.eat('>') {
                    SyntaxKind::Arrow
                } else {
                    SyntaxKind::Eq
                }
            }
            '<' => {
                if self.eat('=') {
                    SyntaxKind::Le
                } else {
                    SyntaxKind::Lt
                }
            }
            '>' => {
                if self.eat('=') {
                    SyntaxKind::Ge
                } else {
                    SyntaxKind::Gt
                }
            }
            '.' => {
                if self.eat('.') {
                    SyntaxKind::DotDot
                } else if self.peek().is_ascii_digit() {
                    self.lex_numeric()
                } else {
                    SyntaxKind::Dot
                }
            }
            '(' => SyntaxKind::OpenParen,
            '[' => SyntaxKind::OpenBrack,
            '{' => SyntaxKind::OpenBrace,
            ')' => SyntaxKind::CloseParen,
            ']' => SyntaxKind::CloseBrack,
            '}' => SyntaxKind::CloseBrace,
            ',' => SyntaxKind::Comma,
            ';' => SyntaxKind::Semi,
            ':' => SyntaxKind::Colon,
            '$' => SyntaxKind::Dollar,
            '`' => SyntaxKind::Backtick,
            '\0' => SyntaxKind::Eof,
            _ => {
                if Self::is_id_start(first) {
                    self.lex_ident(first)
                } else if first.is_whitespace() {
                    self.eat_while(|c| c.is_whitespace());
                    SyntaxKind::Whitespace
                } else {
                    SyntaxKind::Unknown
                }
            }
        };

        let len = (self.pos - start) as u32;
        (len, kind)
    }

    fn lex_ident(&mut self, first: char) -> SyntaxKind {
        let ident = self.accumulate(first, Self::is_id_continue);
        match &ident[..] {
            "as" => SyntaxKind::As,
            "auto" => SyntaxKind::Auto,
            "break" => SyntaxKind::Break,
            "continue" => SyntaxKind::Continue,
            "else" => SyntaxKind::Else,
            "for" => SyntaxKind::For,
            "if" => SyntaxKind::If,
            "import" => SyntaxKind::Import,
            "in" => SyntaxKind::In,
            "include" => SyntaxKind::Include,
            "let" => SyntaxKind::Let,
            "none" => SyntaxKind::None,
            "return" => SyntaxKind::Return,
            "set" => SyntaxKind::Set,
            "show" => SyntaxKind::Show,
            "while" => SyntaxKind::While,
            _ => SyntaxKind::Ident,
        }
    }

    fn lex_numeric(&mut self) -> SyntaxKind {
        self.eat_while(|ch| matches!(ch, '0'..='9'));
        SyntaxKind::Int
    }

    /// Returns `true` and bumps the lexer if the next token
    /// matches the given character.
    fn eat(&mut self, ch: char) -> bool {
        if ch == self.peek() {
            self.bump();
            true
        } else {
            false
        }
    }

    /// Eats character while `matches` returns `true`.
    fn eat_while(&mut self, matches: impl Fn(char) -> bool) {
        while matches(self.peek()) {
            self.bump();
        }
    }

    /// Accumulates characters into a string while `matches` returns `true`.
    fn accumulate(&mut self, ch: char, matches: impl Fn(char) -> bool) -> String {
        let mut string = String::from(ch);
        while let Some(&ch) = self.chars.peek() {
            if !matches(ch) {
                break;
            }

            string.push(ch);
            self.bump();
        }

        string
    }

    /// Returns `true` if the given character can start an identifier.
    fn is_id_start(ch: char) -> bool {
        ch.is_xid_start() || ch == '_'
    }

    /// Returns `true` if the given character can continue an identifier.
    fn is_id_continue(ch: char) -> bool {
        ch.is_xid_continue() || ch == '_' || ch == '-'
    }

    /// Returns the next character without bumping the lexer.
    /// If we have reached EOF, `EOF_CHAR` is returned.
    fn peek(&mut self) -> char {
        *self.chars.peek().unwrap_or(&EOF_CHAR)
    }

    /// Bumps the lexer to the next position.
    fn bump(&mut self) {
        if self.chars.next().is_some() {
            self.pos += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::expect;

    #[test]
    fn test_int() {
        check("1", expect![["Int 1"]]);
        check("123", expect![["Int 3"]]);
    }

    #[test]
    fn test_ident() {
        check("_foo", expect![["Ident 4"]]);
        check("foo-bar", expect![["Ident 7"]]);
        check("_foo_bar123", expect![["Ident 11"]]);
    }

    #[test]
    fn test_keyword() {
        check("as", expect![["As 2"]]);
        check("auto", expect![["Auto 4"]]);
        check("break", expect![["Break 5"]]);
        check("continue", expect![["Continue 8"]]);
        check("else", expect![["Else 4"]]);
        check("for", expect![["For 3"]]);
        check("if", expect![["If 2"]]);
        check("import", expect![["Import 6"]]);
        check("in", expect![["In 2"]]);
        check("include", expect![["Include 7"]]);
        check("let", expect![["Let 3"]]);
        check("none", expect![["None 4"]]);
        check("return", expect![["Return 6"]]);
        check("set", expect![["Set 3"]]);
        check("show", expect![["Show 4"]]);
        check("while", expect![["While 5"]]);
    }

    #[test]
    fn test_op() {
        check("+ /=", expect![["Plus 1Whitespace 1SlashEq 2"]]);
    }

    #[test]
    fn test_whitespace() {
        check(" ", expect![["Whitespace 1"]]);
        check("   \n", expect![["Whitespace 4"]]);
    }

    fn check(input: &str, expect: expect_test::Expect) {
        let actual = lex(input);
        expect.assert_eq(&actual);
    }

    fn lex(src: &str) -> String {
        let mut output = String::new();
        let mut lexer = Lexer::new(src);
        let mut current = lexer.next();

        while current.1 != SyntaxKind::Eof {
            output.push_str(&format!("{:?} {}", current.1, current.0));
            current = lexer.next();
        }

        output
    }
}
