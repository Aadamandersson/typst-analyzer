use crate::{
    kind::SyntaxKind,
    lexer::Lexer,
    untyped::{Element, Node},
};

pub fn parse(src: &str) -> Vec<Element> {
    let lexer = Lexer::new(src);
    let mut parser = Parser::new(src, lexer);
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
    curr_len: u32,
    nodes: Vec<Element>,
    errors: Vec<String>,
}

impl<'s> Parser<'s> {
    fn new(src: &'s str, lexer: Lexer<'s>) -> Self {
        let mut this = Self {
            src,
            lexer,
            curr: SyntaxKind::Eof,
            curr_len: 0,
            nodes: Vec::new(),
            errors: Vec::new(),
        };

        this.bump();
        this
    }

    /// Starts a new node in the syntax tree.
    fn start(&mut self) -> Marker {
        Marker::new(self.nodes.len() as u32)
    }

    /// Completes the node we are currently building.
    /// This wraps all the nodes under `marker` into its own node in the syntax tree.
    fn complete(&mut self, marker: Marker, kind: SyntaxKind) {
        let children = self.nodes.drain(marker.0 as usize..).collect();
        let node = Element::Node(Node::new(kind, children));
        self.nodes.push(node);
    }

    /// We are finished parsing the source text.
    fn finish(self) -> Vec<Element> {
        self.nodes
    }
    
    /// Bumps the parser to the next token.
    fn bump(&mut self) {
        (self.curr_len, self.curr) = self.lexer.next();
    }
}

fn code(parser: &mut Parser) {
    let marker = parser.start();
    

    parser.complete(marker, SyntaxKind::Code);
}

fn code_expr(parser: &mut Parser) {
    todo!()
}
