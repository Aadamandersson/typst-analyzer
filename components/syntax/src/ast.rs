use crate::{
    kind::SyntaxKind,
    node::{SyntaxNode, SyntaxToken},
};

/// A trait for casting an untyped `SyntaxNode` to a typed AST.
pub trait AstNode {
    fn cast(origin: SyntaxNode) -> Option<Self>
    where
        Self: Sized;

    fn origin(&self) -> &SyntaxNode;
}

/// A trait for casting an untyped `SyntaxToken` to a typed token.
pub trait AstToken {
    fn cast(origin: SyntaxToken) -> Option<Self>
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

impl Literal {
    pub fn token(&self) -> SyntaxToken {
        self.0.first_child_or_token().and_then(|n| n.into_token()).unwrap()
    }
}

impl AstNode for Literal {
    fn cast(origin: SyntaxNode) -> Option<Self>
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
    fn cast(origin: SyntaxToken) -> Option<Self>
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
    fn cast(origin: SyntaxToken) -> Option<Self>
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
    fn cast(origin: SyntaxToken) -> Option<Self>
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
    fn cast(origin: SyntaxToken) -> Option<Self>
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_literal_token() {
        let (root, _) = crate::parser::parse(r#"let foo = "bar""#);
        let lit = root.descendants().find_map(Literal::cast).unwrap();
        assert_eq!(r#""bar""#, lit.token().text());
    }
}
