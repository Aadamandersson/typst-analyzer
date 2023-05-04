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

/// A unary operator.
#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    /// `+`
    Pos,
    /// `-`
    Neg,
    /// `not`
    Not,
}

impl UnOp {
    /// Tries to construct a `UnOp` from the given `SyntaxKind`.
    pub fn from_kind(kind: SyntaxKind) -> Option<Self> {
        Some(match kind {
            SyntaxKind::Plus => Self::Pos,
            SyntaxKind::Minus => Self::Neg,
            SyntaxKind::Not => Self::Not,
            _ => return None,
        })
    }

    /// Returns the precedence of this operator.
    pub fn prec(self) -> u8 {
        match self {
            UnOp::Pos | UnOp::Neg => 7,
            UnOp::Not => 4,
        }
    }
}

/// The associativity of a binary operator.
#[derive(Debug, Clone, Copy)]
pub enum Assoc {
    /// The operator is left associative.
    /// E.g., `x + y + z` => `(x + y) + z`
    Left,
    /// The operator is right associative.
    /// E.g., `x = y = z` => `x = (y = z)`
    Right,
}

/// A binary operator.
#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `==`
    Eq,
    /// `!=`
    Ne,
    /// `<`
    Lt,
    /// `<=`
    Le,
    /// `>`
    Gt,
    /// `>=`
    Ge,
    /// `and`
    And,
    /// `or`
    Or,
    /// `in`
    In,
    /// `not in`
    NotIn,
    /// `=`
    Assign,
    /// `+=`
    AddAssign,
    /// `-=`
    SubAssign,
    /// `*=`
    MulAssign,
    /// `/=`
    DivAssign,
}

impl BinOp {
    /// Tries to construct a `BinOp` from the given `SyntaxKind`.
    pub fn from_kind(kind: SyntaxKind) -> Option<Self> {
        Some(match kind {
            SyntaxKind::Plus => Self::Add,
            SyntaxKind::Minus => Self::Sub,
            SyntaxKind::Star => Self::Mul,
            SyntaxKind::Slash => Self::Div,
            SyntaxKind::EqEq => Self::Eq,
            SyntaxKind::Ne => Self::Ne,
            SyntaxKind::Lt => Self::Lt,
            SyntaxKind::Le => Self::Le,
            SyntaxKind::Gt => Self::Gt,
            SyntaxKind::Ge => Self::Ge,
            SyntaxKind::And => Self::And,
            SyntaxKind::Or => Self::Or,
            SyntaxKind::In => Self::In,
            SyntaxKind::Eq => Self::Assign,
            SyntaxKind::PlusEq => Self::AddAssign,
            SyntaxKind::MinusEq => Self::SubAssign,
            SyntaxKind::StarEq => Self::MulAssign,
            SyntaxKind::SlashEq => Self::DivAssign,
            _ => return None,
        })
    }

    /// Returns the precedence of this operator.
    pub fn prec(self) -> u8 {
        match self {
            BinOp::Mul | BinOp::Div => 6,
            BinOp::Add | BinOp::Sub => 5,
            BinOp::Eq
            | BinOp::Ne
            | BinOp::Lt
            | BinOp::Le
            | BinOp::Gt
            | BinOp::Ge
            | BinOp::In
            | BinOp::NotIn => 4,
            BinOp::And => 3,
            BinOp::Or => 2,
            BinOp::Assign
            | BinOp::AddAssign
            | BinOp::SubAssign
            | BinOp::MulAssign
            | BinOp::DivAssign => 1,
        }
    }

    /// Returns the associativity of this binary operator.
    pub fn assoc(self) -> Assoc {
        match self {
            BinOp::Add
            | BinOp::Sub
            | BinOp::Mul
            | BinOp::Div
            | BinOp::Eq
            | BinOp::Ne
            | BinOp::Lt
            | BinOp::Le
            | BinOp::Gt
            | BinOp::Ge
            | BinOp::And
            | BinOp::Or
            | BinOp::In
            | BinOp::NotIn => Assoc::Left,
            BinOp::Assign
            | BinOp::AddAssign
            | BinOp::SubAssign
            | BinOp::MulAssign
            | BinOp::DivAssign => Assoc::Right,
        }
    }
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
                if BinOp::from_kind(tok.kind()).is_some() {
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
