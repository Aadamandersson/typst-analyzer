use std::marker::PhantomData;

use crate::{
    kind::SyntaxKind,
    node::{SyntaxNode, SyntaxNodeChildren, SyntaxToken},
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
    BinaryExpr(BinaryExpr),
}

impl AstNode for Expr {
    fn cast(origin: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        Some(match origin.kind() {
            SyntaxKind::Literal => Expr::Literal(Literal(origin)),
            SyntaxKind::BinaryExpr => Expr::BinaryExpr(BinaryExpr(origin)),
            SyntaxKind::UnaryExpr => todo!(),
            SyntaxKind::ParenExpr => todo!(),
            SyntaxKind::WhileExpr => todo!(),
            SyntaxKind::ForExpr => todo!(),
            SyntaxKind::IfExpr => todo!(),
            SyntaxKind::BreakExpr => todo!(),
            SyntaxKind::ContinueExpr => todo!(),
            SyntaxKind::ArrayExpr => todo!(),
            SyntaxKind::LetBinding => todo!(),
            SyntaxKind::NameRef => todo!(),
            _ => return None,
        })
    }

    fn origin(&self) -> &SyntaxNode {
        match self {
            Expr::Literal(e) => e.origin(),
            Expr::BinaryExpr(e) => e.origin(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Literal(SyntaxNode);

impl Literal {
    pub fn token(&self) -> SyntaxToken {
        self.0
            .first_child_or_token()
            .and_then(|n| n.into_token())
            .unwrap()
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
pub struct ChildIter<N> {
    inner: SyntaxNodeChildren,
    marker: PhantomData<N>,
}

impl<N> ChildIter<N> {
    pub fn new(root: &SyntaxNode) -> Self {
        Self {
            inner: root.children(),
            marker: PhantomData,
        }
    }
}

impl<N: AstNode> Iterator for ChildIter<N> {
    type Item = N;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.find_map(N::cast)
    }
}

fn children<N: AstNode>(root: &SyntaxNode) -> ChildIter<N> {
    ChildIter::new(root)
}

#[derive(Clone, Debug)]
pub struct BinaryExpr(SyntaxNode);

impl BinaryExpr {
    pub fn lhs(&self) -> Option<Expr> {
        children(&self.0).next()
    }

    pub fn op(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find_map(|tok| {
                // TODO: move `BinOp` out from parser
                if crate::parser::BinOp::from_kind(tok.kind()).is_some() {
                    Some(tok)
                } else {
                    None
                }
            })
    }

    pub fn rhs(&self) -> Option<Expr> {
        children(&self.0).nth(1)
    }
}

impl AstNode for BinaryExpr {
    fn cast(origin: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        if origin.kind() == SyntaxKind::BinaryExpr {
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
    fn literal() {
        let (root, _) = crate::parser::parse(r#"let foo = "bar""#);
        let lit = root.descendants().find_map(Literal::cast).unwrap();
        assert_eq!(r#""bar""#, lit.token().text());
    }

    #[test]
    fn binary_expr() {
        let (root, _) = crate::parser::parse("1 + 2");
        let binary = root.descendants().find_map(BinaryExpr::cast).unwrap();
        let lhs = binary.lhs().unwrap().origin().text().to_string();
        let rhs = binary.rhs().unwrap().origin().text().to_string();
        assert_eq!("1", lhs);
        assert_eq!("+", binary.op().unwrap().text());
        assert_eq!("2", rhs);
    }
}
