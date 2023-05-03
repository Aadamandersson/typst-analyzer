use crate::{
    kind::SyntaxKind,
    node::{SyntaxNode, SyntaxToken},
};

/// A trait for casting an untyped `SyntaxNode` to a typed AST.
pub trait AstNode {
    fn cast(&self, origin: SyntaxNode) -> Option<Self>
    where
        Self: Sized;

    fn origin(&self) -> &SyntaxNode;
}

/// A trait for casting an untyped `SyntaxToken` to a typed token.
pub trait AstToken {
    fn cast(&self, origin: SyntaxToken) -> Option<Self>
    where
        Self: Sized;

    fn origin(&self) -> &SyntaxToken;
}

#[derive(Clone, Debug)]
pub enum Expr {
    Literal(Literal),
}

#[derive(Clone, Debug)]
pub struct Literal(SyntaxNode);

impl AstNode for Literal {
    fn cast(&self, origin: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        if origin.kind() == SyntaxKind::Literal {
            Some(Self(origin))
        } else {
            None
        }
    }

    fn origin(&self) -> &SyntaxNode {
        &self.0
    }
}

#[derive(Clone, Debug)]
pub struct Ident(SyntaxToken);

impl AstToken for Ident {
    fn cast(&self, origin: SyntaxToken) -> Option<Self>
    where
        Self: Sized,
    {
        if origin.kind() == SyntaxKind::Ident {
            Some(Self(origin))
        } else {
            None
        }
    }

    fn origin(&self) -> &SyntaxToken {
        &self.0
    }
}

#[derive(Clone, Debug)]
pub struct Int(SyntaxToken);

impl AstToken for Int {
    fn cast(&self, origin: SyntaxToken) -> Option<Self>
    where
        Self: Sized,
    {
        if origin.kind() == SyntaxKind::Int {
            Some(Self(origin))
        } else {
            None
        }
    }

    fn origin(&self) -> &SyntaxToken {
        &self.0
    }
}

#[derive(Clone, Debug)]
pub struct Float(SyntaxToken);

impl AstToken for Float {
    fn cast(&self, origin: SyntaxToken) -> Option<Self>
    where
        Self: Sized,
    {
        if origin.kind() == SyntaxKind::Float {
            Some(Self(origin))
        } else {
            None
        }
    }

    fn origin(&self) -> &SyntaxToken {
        &self.0
    }
}

#[derive(Clone, Debug)]
pub struct String(SyntaxToken);

impl AstToken for String {
    fn cast(&self, origin: SyntaxToken) -> Option<Self>
    where
        Self: Sized,
    {
        if origin.kind() == SyntaxKind::String {
            Some(Self(origin))
        } else {
            None
        }
    }

    fn origin(&self) -> &SyntaxToken {
        &self.0
    }
}
