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
        let node = RawNode::Node(Node::new(kind, children));
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
    code_expr(p);
    p.complete(m, SyntaxKind::Code);
}

fn code_expr(p: &mut Parser) {
    expr1(p);
}

fn expr1(p: &mut Parser) {
    let m = p.start();
    if !literal(p) {
        return;
    }

    loop {
        if p.curr != SyntaxKind::Plus {
            break;
        }
        p.bump();
        if !literal(p) {
            break;
        }
    }

    p.complete(m, SyntaxKind::BinaryExpr);
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
        let src = "123 + 321 + 456";
        let root = parse(src);
        root.dump();
    }
}
