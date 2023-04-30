use crate::{
    kind::SyntaxKind,
    lexer::Lexer,
    node::{Checkpoint, SyntaxError, SyntaxNode, SyntaxTreeBuilder},
};

/// A unary operator.
#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    /// `+`
    Pos,
    /// `-`
    Neg,
    /// `not`
    Not,
}

impl UnOp {
    /// Tries to construct a `UnOp` from the given `SyntaxKind`.
    pub fn from_kind(kind: SyntaxKind) -> Option<Self> {
        Some(match kind {
            SyntaxKind::Plus => Self::Pos,
            SyntaxKind::Minus => Self::Neg,
            SyntaxKind::Not => Self::Not,
            _ => return None,
        })
    }

    /// Returns the precedence of this operator.
    pub fn prec(self) -> u8 {
        match self {
            UnOp::Pos | UnOp::Neg => 7,
            UnOp::Not => 4,
        }
    }
}

/// The associativity of a binary operator.
#[derive(Debug, Clone, Copy)]
pub enum Assoc {
    /// The operator is left associative.
    /// E.g., `x + y + z` => `(x + y) + z`
    Left,
    /// The operator is right associative.
    /// E.g., `x = y = z` => `x = (y = z)`
    Right,
}

/// A binary operator.
#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `==`
    Eq,
    /// `!=`
    Ne,
    /// `<`
    Lt,
    /// `<=`
    Le,
    /// `>`
    Gt,
    /// `>=`
    Ge,
    /// `and`
    And,
    /// `or`
    Or,
    /// `in`
    In,
    /// `not in`
    NotIn,
    /// `=`
    Assign,
    /// `+=`
    AddAssign,
    /// `-=`
    SubAssign,
    /// `*=`
    MulAssign,
    /// `/=`
    DivAssign,
}

impl BinOp {
    /// Tries to construct a `BinOp` from the given `SyntaxKind`.
    pub fn from_kind(kind: SyntaxKind) -> Option<Self> {
        Some(match kind {
            SyntaxKind::Plus => Self::Add,
            SyntaxKind::Minus => Self::Sub,
            SyntaxKind::Star => Self::Mul,
            SyntaxKind::Slash => Self::Div,
            SyntaxKind::EqEq => Self::Eq,
            SyntaxKind::Ne => Self::Ne,
            SyntaxKind::Lt => Self::Lt,
            SyntaxKind::Le => Self::Le,
            SyntaxKind::Gt => Self::Gt,
            SyntaxKind::Ge => Self::Ge,
            SyntaxKind::And => Self::And,
            SyntaxKind::Or => Self::Or,
            SyntaxKind::In => Self::In,
            SyntaxKind::Eq => Self::Assign,
            SyntaxKind::PlusEq => Self::AddAssign,
            SyntaxKind::MinusEq => Self::SubAssign,
            SyntaxKind::StarEq => Self::MulAssign,
            SyntaxKind::SlashEq => Self::DivAssign,
            _ => return None,
        })
    }

    /// Returns the precedence of this operator.
    pub fn prec(self) -> u8 {
        match self {
            BinOp::Mul | BinOp::Div => 6,
            BinOp::Add | BinOp::Sub => 5,
            BinOp::Eq
            | BinOp::Ne
            | BinOp::Lt
            | BinOp::Le
            | BinOp::Gt
            | BinOp::Ge
            | BinOp::In
            | BinOp::NotIn => 4,
            BinOp::And => 3,
            BinOp::Or => 2,
            BinOp::Assign
            | BinOp::AddAssign
            | BinOp::SubAssign
            | BinOp::MulAssign
            | BinOp::DivAssign => 1,
        }
    }

    /// Returns the associativity of this binary operator.
    pub fn assoc(self) -> Assoc {
        match self {
            BinOp::Add
            | BinOp::Sub
            | BinOp::Mul
            | BinOp::Div
            | BinOp::Eq
            | BinOp::Ne
            | BinOp::Lt
            | BinOp::Le
            | BinOp::Gt
            | BinOp::Ge
            | BinOp::And
            | BinOp::Or
            | BinOp::In
            | BinOp::NotIn => Assoc::Left,
            BinOp::Assign
            | BinOp::AddAssign
            | BinOp::SubAssign
            | BinOp::MulAssign
            | BinOp::DivAssign => Assoc::Right,
        }
    }
}

pub fn parse(src: &str) -> (SyntaxNode, Vec<SyntaxError>) {
    let mut parser = Parser::new(src);
    code(&mut parser);
    parser.finish()
}

struct Parser<'s> {
    src: &'s str,
    lexer: Lexer<'s>,
    curr: SyntaxKind,
    curr_len: usize,
    curr_start: usize,
    builder: SyntaxTreeBuilder,
}

impl<'s> Parser<'s> {
    fn new(src: &'s str) -> Self {
        let mut lexer = Lexer::new(src);
        let curr_start = lexer.pos();
        let (curr_len, curr) = lexer.next();
        Self {
            src,
            lexer,
            curr,
            curr_len,
            curr_start,
            builder: SyntaxTreeBuilder::new(),
        }
    }

    /// Starts a new node in the syntax tree.
    fn start(&mut self, kind: SyntaxKind) {
        self.eat_trivia();
        self.builder.start_node(kind);
        self.eat_trivia();
    }

    /// Starts a new node in the syntax tree and the given checkpoint.
    fn start_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) {
        self.builder.start_node_at(checkpoint, kind);
    }

    /// Prepare for maybe wrapping the next node.
    fn checkpoint(&mut self) -> Checkpoint {
        self.eat_trivia();
        let cp = self.builder.checkpoint();
        self.eat_trivia();
        cp
    }

    /// Finish up and wrap the branch we have currently been building.
    fn wrap(&mut self) {
        self.builder.finish_node();
    }

    /// We are finished parsing the source text.
    fn finish(self) -> (SyntaxNode, Vec<SyntaxError>) {
        self.builder.finish()
    }

    /// Bumps the parser to the next non-trivia token.
    fn eat_trivia(&mut self) {
        while self.curr.is_trivia() {
            self.next();
        }
    }

    /// Adds the current token to the branch we are building and
    /// bumps the parser to the next token.
    fn bump(&mut self) {
        if self.at(SyntaxKind::Eof) {
            return;
        }

        self.eat_trivia();
        self.next();
    }

    fn next(&mut self) {
        let text = &self.src[self.curr_start..(self.curr_start + self.curr_len)];
        self.builder.token(self.curr, text);
        self.curr_start = self.lexer.pos();
        (self.curr_len, self.curr) = self.lexer.next();
    }

    /// Returns `true` and eats the next token if the current `SyntaxKind` is `kind`, otherwise returns `false`.
    fn eat(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    /// Eats the next token if it is `kind`, otherwise emits an error.
    fn expect(&mut self, kind: SyntaxKind) {
        if self.eat(kind) {
            return;
        }
        self.error(format!("expected `{kind:?}`"));
    }

    /// Returns `true` if the current `SyntaxKind` is `kind`, otherwise returns `false`.
    fn at(&self, kind: SyntaxKind) -> bool {
        self.curr == kind
    }

    fn error(&mut self, msg: impl ToString) {
        self.builder.error(msg, self.curr_start.try_into().unwrap());
    }
}

fn code(p: &mut Parser) {
    p.start(SyntaxKind::Code);
    while !p.at(SyntaxKind::Eof) {
        // We always want to construct a syntax tree, so just bump the parser
        // if we encounter something that we have not yet implemented support for.
        if !code_expr(p) {
            p.bump();
        }
    }

    p.wrap();
}

fn code_expr(p: &mut Parser) -> bool {
    match p.curr {
        SyntaxKind::Let => let_binding(p),
        _ => code_prec_expr(p, 0),
    }
}

fn let_binding(p: &mut Parser) -> bool {
    p.start(SyntaxKind::LetBinding);
    p.bump();
    p.eat_trivia();

    let mut is_fn = false;
    if p.at(SyntaxKind::Ident) {
        let cp = p.checkpoint();
        p.bump();

        if p.at(SyntaxKind::OpenParen) {
            p.start_at(cp, SyntaxKind::FnPat);
            params(p);
            is_fn = true;
        } else {
            p.start_at(cp, SyntaxKind::IdentPat);
        }

        p.wrap();
    } else if p.at(SyntaxKind::Underscore) {
        p.start(SyntaxKind::WildcardPat);
        p.bump();
        p.wrap();
    } else {
        p.error("expected pattern");
    }

    p.eat_trivia();
    if is_fn {
        p.expect(SyntaxKind::Eq);
        if !code_expr(p) {
            p.error("expected expression");
        }
    } else {
        if p.eat(SyntaxKind::Eq) && !code_expr(p) {
            p.error("expected expression");
        }
    }

    p.wrap();
    true
}

fn params(p: &mut Parser) {
    p.start(SyntaxKind::Params);
    p.bump();

    while !p.at(SyntaxKind::CloseParen) && !p.at(SyntaxKind::Eof) {
        param(p);
        if !p.eat(SyntaxKind::Comma) {
            break;
        }
    }

    p.expect(SyntaxKind::CloseParen);
    p.wrap();
}

fn param(p: &mut Parser) {
    p.start(SyntaxKind::Param);
    // TODO: support more than `IdentPat`
    p.start(SyntaxKind::IdentPat);
    name(p);
    p.wrap();
    p.wrap();
}

// TODO: `BinOp::NotIn`
fn code_prec_expr(p: &mut Parser, min_prec: u8) -> bool {
    let cp = p.checkpoint();
    if let Some(op) = UnOp::from_kind(p.curr) {
        p.start(SyntaxKind::UnaryExpr);
        p.bump();
        if !code_prec_expr(p, op.prec()) {
            p.error("expected expression");
        }
        p.wrap();
    } else if !code_primary_expr(p) {
        return false;
    }

    p.eat_trivia();

    loop {
        let Some(op) = BinOp::from_kind(p.curr) else {
            break;
        };

        if op.prec() < min_prec {
            break;
        }

        p.bump();

        let prec = match op.assoc() {
            Assoc::Left => op.prec() + 1,
            Assoc::Right => op.prec(),
        };

        if !code_prec_expr(p, prec) {
            p.error("expected expression");
        }

        p.start_at(cp, SyntaxKind::BinaryExpr);
        p.wrap();
    }

    true
}

fn code_primary_expr(p: &mut Parser) -> bool {
    match p.curr {
        SyntaxKind::Ident => name_ref(p),
        SyntaxKind::OpenParen => parenthesized(p),
        SyntaxKind::OpenBrack => content_block(p),
        SyntaxKind::OpenBrace => code_block(p),
        SyntaxKind::Int | SyntaxKind::Float | SyntaxKind::String => literal(p),
        _ => return false,
    }
    true
}

fn name(p: &mut Parser) {
    p.start(SyntaxKind::Name);
    p.expect(SyntaxKind::Ident);
    p.wrap();
}

fn name_ref(p: &mut Parser) {
    p.start(SyntaxKind::NameRef);
    p.bump();
    p.wrap();
}

// TODO: dictionary
fn parenthesized(p: &mut Parser) {
    let cp = p.checkpoint();
    p.bump();

    let mut elements = 0;
    while !p.at(SyntaxKind::Eof) && !p.at(SyntaxKind::CloseParen) {
        if !code_expr(p) {
            p.error("expected expression");
        }

        elements += 1;

        if !p.eat(SyntaxKind::Comma) {
            break;
        }
    }

    p.expect(SyntaxKind::CloseParen);
    if elements == 1 {
        p.start_at(cp, SyntaxKind::ParenExpr)
    } else {
        p.start_at(cp, SyntaxKind::ArrayExpr)
    }

    p.wrap();
}

fn content_block(p: &mut Parser) {
    p.start(SyntaxKind::ContentBlock);
    p.expect(SyntaxKind::OpenBrack);

    while !p.at(SyntaxKind::Eof) && !p.at(SyntaxKind::CloseBrack) {
        // Since we do not _parse_ markup yet, just eat the body
        p.bump();
    }

    p.expect(SyntaxKind::CloseBrack);
    p.wrap();
}

fn code_block(p: &mut Parser) {
    p.start(SyntaxKind::CodeBlock);
    p.expect(SyntaxKind::OpenBrace);

    while !p.at(SyntaxKind::Eof) && !p.at(SyntaxKind::CloseBrace) {
        code_expr(p);
    }

    p.expect(SyntaxKind::CloseBrace);
    p.wrap();
}

fn literal(p: &mut Parser) {
    p.start(SyntaxKind::Literal);
    p.bump();
    p.wrap();
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::expect_file;
    use std::{env, fs, path::Path};

    #[test]
    fn test_parse_ok() {
        parse_test_dir("ok")
    }

    #[test]
    fn test_parse_err() {
        parse_test_dir("err")
    }

    fn parse_test_dir(dir: &str) {
        let root_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
        let data_dir = root_dir.join(format!("test_data/parser/{}", dir));
        let read_dir =
            fs::read_dir(&data_dir).expect(&format!("could not read `{}`", data_dir.display()));

        for file in read_dir {
            let file = file.unwrap();
            let path = file.path();
            if path.extension().unwrap_or_default() == "typ" {
                let ast = path.with_extension("ast");
                let src = fs::read_to_string(&path)
                    .expect(&format!("could not read `{}`", path.display()));
                let (root, errors) = parse(&src);
                let actual = format!("{:#?}", root);
                let mut actual_with_errors = actual;
                for error in errors {
                    actual_with_errors.push_str(&format!(
                        "error: {:?}: {}\n",
                        error.range().start(),
                        error.message()
                    ));
                }
                expect_file![ast].assert_eq(&actual_with_errors);
            }
        }
    }
}
