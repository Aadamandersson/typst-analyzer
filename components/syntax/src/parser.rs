use crate::{
    ast::{Assoc, BinOp, UnOp},
    kind::SyntaxKind,
    lexer::Lexer,
    node::{Checkpoint, SyntaxError, SyntaxNode, SyntaxTreeBuilder},
};

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
        // TODO: handle this properly when we support parsing markup.
        p.eat_trivia();
        if p.at(SyntaxKind::Eof) {
            break;
        }

        if p.at(SyntaxKind::Pound) {
            p.bump();
        }

        let prev = p.curr;
        code_expr(p);
        // We always want to construct a syntax tree, so just bump the parser
        // if we encounter something that we have not yet implemented support for.
        // Eventually, we want to report an error here.
        if prev == p.curr {
            p.bump();
        }
    }

    p.wrap();
}

fn code_expr(p: &mut Parser) {
    match p.curr {
        SyntaxKind::Let => let_binding(p),
        SyntaxKind::While => while_expr(p),
        SyntaxKind::For => for_expr(p),
        SyntaxKind::Continue => continue_expr(p),
        SyntaxKind::Break => break_expr(p),
        _ => code_prec_expr(p, 0),
    }
}

fn let_binding(p: &mut Parser) {
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
        code_expr(p);
    } else if p.eat(SyntaxKind::Eq) {
        code_expr(p);
    }

    p.wrap();
}

fn while_expr(p: &mut Parser) {
    p.start(SyntaxKind::WhileExpr);

    p.bump();
    code_expr(p);
    block(p);

    p.wrap();
}

fn for_expr(p: &mut Parser) {
    p.start(SyntaxKind::ForExpr);

    p.bump();

    let cp = p.checkpoint();
    // TODO: support destructuring syntax
    if p.eat(SyntaxKind::Ident) {
        p.start_at(cp, SyntaxKind::IdentPat);
        p.wrap();
    } else if p.eat(SyntaxKind::Underscore) {
        p.start_at(cp, SyntaxKind::WildcardPat);
        p.wrap();
    } else {
        p.error("expected pattern");
    }

    p.eat_trivia();
    p.expect(SyntaxKind::In);
    code_expr(p);
    block(p);

    p.wrap();
}

fn block(p: &mut Parser) {
    match p.curr {
        SyntaxKind::OpenBrack => content_block(p),
        SyntaxKind::OpenBrace => code_block(p),
        _ => p.error("expected block"),
    }
}

fn continue_expr(p: &mut Parser) {
    p.start(SyntaxKind::ContinueExpr);
    p.bump();
    p.wrap();
}

fn break_expr(p: &mut Parser) {
    p.start(SyntaxKind::BreakExpr);
    p.bump();
    p.wrap();
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
fn code_prec_expr(p: &mut Parser, min_prec: u8) {
    let cp = p.checkpoint();
    if let Some(op) = UnOp::from_kind(p.curr) {
        p.start(SyntaxKind::UnaryExpr);
        p.bump();
        code_prec_expr(p, op.prec());
        p.wrap();
    } else {
        code_primary_expr(p);
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

        code_prec_expr(p, prec);
        p.start_at(cp, SyntaxKind::BinaryExpr);
        p.wrap();
    }
}

fn code_primary_expr(p: &mut Parser) {
    match p.curr {
        SyntaxKind::Ident => name_ref(p),
        SyntaxKind::If => if_expr(p),
        SyntaxKind::OpenParen => parenthesized(p),
        SyntaxKind::OpenBrack => content_block(p),
        SyntaxKind::OpenBrace => code_block(p),
        SyntaxKind::Int
        | SyntaxKind::Float
        | SyntaxKind::String
        | SyntaxKind::True
        | SyntaxKind::False => literal(p),
        _ => p.error("expected expression"),
    }
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

fn if_expr(p: &mut Parser) {
    p.start(SyntaxKind::IfExpr);

    p.bump();
    code_expr(p);
    block(p);

    p.eat_trivia();
    if p.eat(SyntaxKind::Else) {
        p.eat_trivia();
        if p.at(SyntaxKind::If) {
            if_expr(p);
        } else {
            block(p);
        }
    }

    p.wrap();
}

// TODO: dictionary
fn parenthesized(p: &mut Parser) {
    let cp = p.checkpoint();
    p.bump();

    let mut elements = 0;
    while !p.at(SyntaxKind::Eof) && !p.at(SyntaxKind::CloseParen) {
        code_expr(p);
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
