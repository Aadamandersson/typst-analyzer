use crate::kind::SyntaxKind;

pub enum Element {
    Node(Node),
    Leaf(Token),
}

impl Element {
    pub(crate) fn len(&self) -> usize {
        match self {
            Element::Node(n) => n.text_len(),
            Element::Leaf(t) => t.text_len(),
        }
    }
    
    pub(crate) fn dump(&self) {
        self.preorder(1)
    }

    fn preorder(&self, depth: u32) {
        match self {
            Element::Node(n) => {
                println!("{:?}", n.kind());
                for child in n.children() {
                    for _ in 0..depth {
                        print!("  ");
                    }
                    child.preorder(depth + 1);
                }
            }
            Element::Leaf(t) => {
                println!("{:?}", t);
            }
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

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {:?}", self.kind, self.text)
    }
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
