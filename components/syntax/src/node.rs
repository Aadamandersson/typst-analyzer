use crate::kind::SyntaxKind;
use rowan::{GreenNodeBuilder, Language, TextRange, TextSize};

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
// pub type SyntaxElement = rowan::SyntaxToken<TypstLanguage>;
pub type Checkpoint = rowan::Checkpoint;

#[derive(Debug, Clone)]
pub struct SyntaxError(String, TextRange);

impl SyntaxError {
    /// Constructs a new `SyntaxError` with the specified message and range.
    pub fn new(msg: impl ToString, range: TextRange) -> Self {
        Self(msg.to_string(), range)
    }

    /// Constructs a new `SyntaxError` with the specified message and range (`offset..offset`).
    pub fn at_offset(msg: impl ToString, offset: TextSize) -> Self {
        Self(msg.to_string(), TextRange::empty(offset))
    }

    pub fn message(&self) -> &str {
        &self.0
    }

    pub fn range(&self) -> TextRange {
        self.1
    }
}

pub(crate) struct SyntaxTreeBuilder {
    inner: GreenNodeBuilder<'static>,
    errors: Vec<SyntaxError>,
}

impl SyntaxTreeBuilder {
    pub(crate) fn new() -> Self {
        Self {
            inner: GreenNodeBuilder::new(),
            errors: Vec::new(),
        }
    }

    pub fn finish(self) -> (SyntaxNode, Vec<SyntaxError>) {
        (SyntaxNode::new_root(self.inner.finish()), self.errors)
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

    pub fn error(&mut self, msg: impl ToString, offset: TextSize) {
        self.errors.push(SyntaxError::at_offset(msg, offset));
    }
}
