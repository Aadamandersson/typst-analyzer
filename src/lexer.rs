use expect_test::expect;
use std::{iter::Peekable, str::Chars};

/// Represents all possible syntactic constructs for `Typst`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum SyntaxKind {
    /// An integer.
    /// E.g., `123`.
    Int,
    /// E.g., (' ', '\t', '\n', etc...)
    Whitespace,
    /// An unknown character to the lexer.
    Unknown,
    /// End of file.
    Eof,
}

/// Takes the source text and splits it into tokens.
struct Lexer<'s> {
    /// The characters in the source text.
    chars: Peekable<Chars<'s>>,
    /// The current position in the source text.
    pos: usize,
}

impl<'s> Lexer<'s> {
    /// Constructs a new `Lexer` with the given source text.
    fn new(src: &'s str) -> Self {
        let chars = src.chars().peekable();
        Self { chars, pos: 0 }
    }

    /// Returns the next `SyntaxKind` and its length.
    fn next(&mut self) -> (u32, SyntaxKind) {
        if let Some(&ch) = self.chars.peek() {
            let start = self.pos;
            self.bump();
            let kind = match ch {
                '0'..='9' => self.lex_numeric(),
                _ => {
                    if ch.is_whitespace() {
                        self.eat_while(|c| c.is_whitespace());
                        SyntaxKind::Whitespace
                    } else {
                        SyntaxKind::Unknown
                    }
                }
            };

            let len = (self.pos - start) as u32;
            (len, kind)
        } else {
            (0, SyntaxKind::Eof)
        }
    }

    /// Lexes a numerical value.
    fn lex_numeric(&mut self) -> SyntaxKind {
        self.eat_while(|ch| matches!(ch, '0'..='9'));
        SyntaxKind::Int
    }

    /// Eats character while `matches` returns `true`.
    fn eat_while(&mut self, matches: impl Fn(char) -> bool) {
        while let Some(&ch) = self.chars.peek() {
            if !matches(ch) {
                break;
            }
            self.bump();
        }
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

    #[test]
    fn test_int() {
        check("1", expect![["Int 1"]]);
        check("123", expect![["Int 3"]]);
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
