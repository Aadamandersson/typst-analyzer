use crate::{
    kind::SyntaxKind,
    lexer::Lexer,
    untyped::{Node, RawNode, Token},
};

pub fn parse(src: &str) -> RawNode {
    let mut parser = Parser::new(src);
    code(&mut parser);
    parser.finish().into_iter().next().unwrap()
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
    nodes: Vec<RawNode>,
    errors: Vec<String>,
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
            nodes: Vec::new(),
            errors: Vec::new(),
        }
    }

    /// Starts a new node in the syntax tree.
    fn start(&mut self) -> Marker {
        Marker::new(self.nodes.len() as u32)
    }

    /// Completes the node we are currently building.
    /// This wraps all the nodes under `marker` into its own node in the syntax tree.
    fn complete(&mut self, marker: Marker, kind: SyntaxKind) {
        let children = self.nodes.drain(marker.0 as usize..).collect();
        let node = RawNode::Inner(Node::new(kind, children));
        self.nodes.push(node);
    }

    /// We are finished parsing the source text.
    fn finish(self) -> Vec<RawNode> {
        self.nodes
    }

    /// Returns the text for the current token.
    fn current_text(&self) -> &str {
        &self.src[self.curr_start..(self.curr_start + self.curr_len)]
    }

    /// Bumps the parser to the next non-trivia token.
    fn bump(&mut self) {
        if self.curr == SyntaxKind::Eof {
            return;
        }

        let node = RawNode::Leaf(Token::new(self.curr, self.current_text().to_owned()));
        self.nodes.push(node);
        self.curr_start = self.lexer.pos();
        (self.curr_len, self.curr) = self.lexer.next();

        if self.curr.is_trivia() {
            self.bump();
        }
    }
}

fn code(p: &mut Parser) {
    let m = p.start();
    while p.curr != SyntaxKind::Eof {
        code_expr(p);
    }
    p.complete(m, SyntaxKind::Code);
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
    let m = p.start();
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

    p.complete(m, SyntaxKind::BinaryExpr);
    true
}

fn expr2(p: &mut Parser) -> bool {
    let m = p.start();
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

    p.complete(m, SyntaxKind::BinaryExpr);
    true
}

fn expr1(p: &mut Parser) -> bool {
    match p.curr {
        SyntaxKind::Plus | SyntaxKind::Minus => {
            let m = p.start();
            p.bump();
            expr1(p);
            p.complete(m, SyntaxKind::UnaryExpr);
            true
        }
        _ => literal(p),
    }
}

fn literal(p: &mut Parser) -> bool {
    if p.curr != SyntaxKind::Int && p.curr != SyntaxKind::Float && p.curr != SyntaxKind::String {
        return false;
    }

    let m = p.start();
    p.bump();
    p.complete(m, SyntaxKind::Literal);
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn go() {
        let src = "-1 + 2 * 3";
        let root = parse(src);
        root.dump();
    }
}
