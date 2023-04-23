use rowan::{GreenNodeBuilder, Language};

use crate::kind::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypstLanguage {}

impl Language for TypstLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        SyntaxKind::from(raw.0)
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.into())
    }
}

pub type SyntaxNode = rowan::SyntaxNode<TypstLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<TypstLanguage>;
pub type SyntaxElement = rowan::SyntaxToken<TypstLanguage>;
pub type Checkpoint = rowan::Checkpoint;

pub(crate) struct SyntaxTreeBuilder {
    inner: GreenNodeBuilder<'static>,
}

impl SyntaxTreeBuilder {
    pub(crate) fn new() -> Self {
        Self {
            inner: GreenNodeBuilder::new(),
        }
    }

    pub fn finish(self) -> SyntaxNode {
        SyntaxNode::new_root(self.inner.finish())
    }

    pub fn checkpoint(&self) -> Checkpoint {
        self.inner.checkpoint()
    }

    pub fn token(&mut self, kind: SyntaxKind, text: &str) {
        let kind = TypstLanguage::kind_to_raw(kind);
        self.inner.token(kind, text);
    }

    pub fn start_node(&mut self, kind: SyntaxKind) {
        let kind = TypstLanguage::kind_to_raw(kind);
        self.inner.start_node(kind);
    }

    pub fn start_node_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) {
        let kind = TypstLanguage::kind_to_raw(kind);
        self.inner.start_node_at(checkpoint, kind)
    }

    pub fn finish_node(&mut self) {
        self.inner.finish_node();
    }
}
