use crate::{
    kind::SyntaxKind,
    lexer::Lexer,
    node::{Checkpoint, SyntaxNode, SyntaxTreeBuilder},
};

pub fn parse(src: &str) -> SyntaxNode {
    let mut parser = Parser::new(src);
    code(&mut parser);
    parser.finish()
}

#[derive(Debug, Clone, Copy)]
struct Marker(u32);
impl Marker {
    fn new(pos: u32) -> Self {
        Self(pos)
    }
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
        self.builder.start_node(kind);
    }

    /// Starts a new node in the syntax tree and the given checkpoint.
    fn start_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) {
        self.builder.start_node_at(checkpoint, kind)
    }

    /// Prepare for maybe wrapping the next node.
    fn checkpoint(&self) -> Checkpoint {
        self.builder.checkpoint()
    }

    /// Finish up and wrap the branch we have currently been building.
    fn wrap(&mut self) {
        self.builder.finish_node();
    }

    /// We are finished parsing the source text.
    fn finish(self) -> SyntaxNode {
        self.builder.finish()
    }

    /// Returns the text for the current token.
    fn current_text(&self) -> &str {
        &self.src[self.curr_start..(self.curr_start + self.curr_len)]
    }

    /// Bumps the parser to the next non-trivia token.
    fn eat_trivia(&mut self) {
        while self.curr.is_trivia() {
            self.bump();
        }
    }

    /// Bumps the parser to the next token.
    fn bump(&mut self) {
        if self.curr == SyntaxKind::Eof {
            return;
        }

        let text = &self.src[self.curr_start..(self.curr_start + self.curr_len)];
        self.builder.token(self.curr, text);
        self.curr_start = self.lexer.pos();
        (self.curr_len, self.curr) = self.lexer.next();
    }
}

fn code(p: &mut Parser) {
    p.start(SyntaxKind::Code);
    while p.curr != SyntaxKind::Eof {
        code_expr(p);
    }

    p.wrap();
}

fn code_expr(p: &mut Parser) {
    expr3(p);
}

// TODO:
//  * expr6: =, +=, -=, *=, /=,
//  * expr5: And, Or
//  * expr4: Eq, Ne, Lt, Le, Gt, Ge, In, Not, NotIn,
//
//  eventually replace with e.g., precedence climbing

fn expr3(p: &mut Parser) -> bool {
    let cp = p.checkpoint();
    expr2(p);

    if p.curr != SyntaxKind::Plus && p.curr != SyntaxKind::Minus {
        return false;
    }

    while p.curr == SyntaxKind::Plus || p.curr == SyntaxKind::Minus {
        p.bump();
        if !expr2(p) {
            break;
        }
    }

    p.start_at(cp, SyntaxKind::BinaryExpr);
    p.wrap();
    true
}

fn expr2(p: &mut Parser) -> bool {
    let cp = p.checkpoint();
    expr1(p);

    if p.curr != SyntaxKind::Star && p.curr != SyntaxKind::Slash {
        return false;
    }

    while p.curr == SyntaxKind::Star || p.curr == SyntaxKind::Slash {
        p.bump();
        if !expr1(p) {
            break;
        }
    }

    p.start_at(cp, SyntaxKind::BinaryExpr);
    p.wrap();
    true
}

fn expr1(p: &mut Parser) -> bool {
    match p.curr {
        SyntaxKind::Plus | SyntaxKind::Minus => {
            p.start(SyntaxKind::UnaryExpr);
            p.bump();
            expr1(p);
            p.wrap();
            true
        }
        _ => literal(p),
    }
}

fn literal(p: &mut Parser) -> bool {
    if p.curr != SyntaxKind::Int && p.curr != SyntaxKind::Float && p.curr != SyntaxKind::String {
        return false;
    }

    p.start(SyntaxKind::Literal);
    p.bump();
    p.wrap();
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn go() {
        let src = "-1+2*3";
        let root = parse(src);
        println!("{:#?}", root);
    }
}
