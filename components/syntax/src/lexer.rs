use std::{iter::Peekable, str::Chars};
use unicode_xid::UnicodeXID;

use crate::{kind::SyntaxKind, Token};

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

    /// Returns the current position in the source text.
    pub(crate) fn pos(&self) -> usize {
        self.pos
    }

    /// Returns the next `SyntaxKind` and its length.
    pub(crate) fn next(&mut self) -> (usize, SyntaxKind) {
        let first = self.peek();
        let start = self.pos;
        self.bump();
        let kind = match first {
            '0'..='9' => self.lex_numeric(first),
            '"' => self.lex_string(),
            '+' => {
                if self.eat('=') {
                    Token![+=]
                } else {
                    Token![+]
                }
            }
            '-' => {
                if self.eat('=') {
                    Token![-=]
                } else {
                    Token![-]
                }
            }
            '*' => {
                if self.eat('=') {
                    Token![*=]
                } else {
                    Token![*]
                }
            }
            '/' => {
                if self.eat('=') {
                    Token![/=]
                } else if self.eat('/') {
                    self.lex_line_comment()
                } else if self.eat('*') {
                    self.lex_block_comment()
                } else {
                    Token![/]
                }
            }
            '!' if self.eat('=') => Token![!=],
            '=' => {
                if self.eat('=') {
                    Token![==]
                } else if self.eat('>') {
                    Token![=>]
                } else {
                    Token![=]
                }
            }
            '<' => {
                if self.eat('=') {
                    Token![<=]
                } else {
                    Token![<]
                }
            }
            '>' => {
                if self.eat('=') {
                    Token![>=]
                } else {
                    Token![>]
                }
            }
            '.' => {
                if self.eat('.') {
                    Token![..]
                } else if self.peek().is_ascii_digit() {
                    self.lex_numeric(first)
                } else {
                    Token![.]
                }
            }
            '(' => Token!['('],
            '[' => Token!['['],
            '{' => Token!['{'],
            ')' => Token![')'],
            ']' => Token![']'],
            '}' => Token!['}'],
            ',' => Token![,],
            ';' => Token![;],
            ':' => Token![:],
            '$' => Token![$],
            '#' => Token![#],
            '`' => Token!['`'],
            '\0' => Token![eof],
            _ => {
                if Self::is_id_start(first) {
                    self.lex_ident(first)
                } else if first.is_whitespace() {
                    self.eat_while(|c| c.is_whitespace());
                    Token![whitespace]
                } else {
                    SyntaxKind::Error
                }
            }
        };

        let len = self.pos - start;
        (len, kind)
    }

    fn lex_ident(&mut self, first: char) -> SyntaxKind {
        let ident = self.accumulate(first, Self::is_id_continue);
        match &ident[..] {
            "_" => Token![_],
            "and" => Token![and],
            "as" => Token![as],
            "auto" => Token![auto],
            "break" => Token![break],
            "continue" => Token![continue],
            "else" => Token![else],
            "false" => Token![false],
            "for" => Token![for],
            "if" => Token![if],
            "import" => Token![import],
            "in" => Token![in],
            "include" => Token![include],
            "let" => Token![let],
            "none" => Token![none],
            "not" => Token![not],
            "or" => Token![or],
            "return" => Token![return],
            "set" => Token![set],
            "show" => Token![show],
            "true" => Token![true],
            "while" => Token![while],
            _ => Token![ident],
        }
    }

    fn lex_numeric(&mut self, first: char) -> SyntaxKind {
        // TODO:
        // * binary and octal bases
        // * exponents & suffixes
        // * error reporting

        if first == '.' {
            self.eat_while(|ch| ch.is_ascii_digit());
            return Token![float];
        }

        self.eat_while(|ch| ch.is_ascii_digit());
        if self.eat('.') {
            self.eat_while(|ch| ch.is_ascii_digit());
            return Token![float];
        }

        Token![int]
    }

    fn lex_string(&mut self) -> SyntaxKind {
        // TODO: support escape?
        self.eat_while(|ch| ch != '"');
        if !self.eat('"') {
            // TODO: report error
            SyntaxKind::Error
        } else {
            Token![string]
        }
    }

    fn lex_line_comment(&mut self) -> SyntaxKind {
        self.eat_while(|ch| ch != '\r' && ch != '\n');
        Token![comment]
    }

    fn lex_block_comment(&mut self) -> SyntaxKind {
        // TODO: report error if not terminated
        //       support nested block comments?
        while let Some(&ch) = self.chars.peek() {
            if ch == '*' {
                self.bump();
                if self.eat('/') {
                    break;
                }
            }
            self.bump();
        }
        Token![comment]
    }

    /// Returns `true` and bumps the lexer if the next token
    /// is the given character.
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
        while let Some(&ch) = self.chars.peek() {
            if !matches(ch) {
                break;
            }

            self.bump();
        }
    }

    /// Accumulates characters into a string while `matches` returns `true`.
    fn accumulate(&mut self, first: char, matches: impl Fn(char) -> bool) -> String {
        let mut string = String::from(first);

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
    fn test_ident() {
        check("_foo", expect![["Ident 4"]]);
        check("foo-bar", expect![["Ident 7"]]);
        check("_foo_bar123", expect![["Ident 11"]]);
    }

    #[test]
    fn test_number() {
        check("1", expect![["Int 1"]]);
        check("123", expect![["Int 3"]]);
        check(".12", expect![["Float 3"]]);
        check("0.1", expect![["Float 3"]]);
        check("1.23", expect![["Float 4"]]);
    }

    #[test]
    fn test_string() {
        check(r#""foo""#, expect![["String 5"]]);
        check(r#""foo" "bar""#, expect![["String 5Whitespace 1String 5"]]);
        check(r#""foo"#, expect![["Error 4"]]);
    }

    #[test]
    fn test_keyword() {
        check("_", expect![["Underscore 1"]]);
        check("and", expect![["And 3"]]);
        check("as", expect![["As 2"]]);
        check("auto", expect![["Auto 4"]]);
        check("break", expect![["Break 5"]]);
        check("continue", expect![["Continue 8"]]);
        check("else", expect![["Else 4"]]);
        check("false", expect![["False 5"]]);
        check("for", expect![["For 3"]]);
        check("if", expect![["If 2"]]);
        check("import", expect![["Import 6"]]);
        check("in", expect![["In 2"]]);
        check("include", expect![["Include 7"]]);
        check("let", expect![["Let 3"]]);
        check("none", expect![["None 4"]]);
        check("not", expect![["Not 3"]]);
        check("or", expect![["Or 2"]]);
        check("return", expect![["Return 6"]]);
        check("set", expect![["Set 3"]]);
        check("show", expect![["Show 4"]]);
        check("true", expect![["True 4"]]);
        check("while", expect![["While 5"]]);
    }

    #[test]
    fn test_op() {
        check("+ /=", expect![["Plus 1Whitespace 1SlashEq 2"]]);
    }

    #[test]
    fn test_comment() {
        check("//", expect![["Comment 2"]]);
        check("// abc", expect![["Comment 6"]]);
        check("/**/", expect![["Comment 4"]]);
        check("/* abc */", expect![["Comment 9"]]);
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

        while current.1 != Token![eof] {
            output.push_str(&format!("{:?} {}", current.1, current.0));
            current = lexer.next();
        }

        output
    }
}
