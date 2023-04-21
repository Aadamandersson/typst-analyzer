use crate::kind::SyntaxKind;

pub enum Element {
    Node(Node),
    Leaf(Token),
}

impl Element {
    fn len(&self) -> usize {
        match self {
            Element::Node(n) => n.text_len(),
            Element::Leaf(t) => t.text_len(),
        }
    }
}

pub struct Node {
    kind: SyntaxKind,
    children: Vec<Element>,
    len: usize,
}

impl Node {
    pub(crate) fn new(kind: SyntaxKind, children: Vec<Element>) -> Self {
        let len = children.iter().map(|e| e.len()).sum();
        Self {
            kind,
            children,
            len,
        }
    }

    pub(crate) fn kind(&self) -> SyntaxKind {
        self.kind
    }

    pub(crate) fn children(&self) -> impl Iterator<Item = &Element> {
        self.children.iter()
    }

    pub(crate) fn text_len(&self) -> usize {
        self.len
    }
}

pub struct Token {
    kind: SyntaxKind,
    text: String,
}

impl Token {
    pub(crate) fn new(kind: SyntaxKind, text: String) -> Self {
        Self { kind, text }
    }

    pub(crate) fn kind(&self) -> SyntaxKind {
        self.kind
    }

    pub(crate) fn text_len(&self) -> usize {
        self.text.len()
    }
}
