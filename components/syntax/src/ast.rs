use std::marker::PhantomData;

use crate::{
    kind::SyntaxKind,
    node::{SyntaxNode, SyntaxNodeChildren, SyntaxToken},
    Token,
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
            Token![+] => Self::Pos,
            Token![-] => Self::Neg,
            Token![not] => Self::Not,
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
            Token![+] => Self::Add,
            Token![-] => Self::Sub,
            Token![*] => Self::Mul,
            Token![/] => Self::Div,
            Token![==] => Self::Eq,
            Token![!=] => Self::Ne,
            Token![<] => Self::Lt,
            Token![<=] => Self::Le,
            Token![>] => Self::Gt,
            Token![>=] => Self::Ge,
            Token![and] => Self::And,
            Token![or] => Self::Or,
            Token![in] => Self::In,
            Token![=] => Self::Assign,
            Token![+=] => Self::AddAssign,
            Token![-=] => Self::SubAssign,
            Token![*=] => Self::MulAssign,
            Token![/=] => Self::DivAssign,
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
pub enum Pat {
    IdentPat(IdentPat),
    WildcardPat(WildcardPat),
}

impl AstNode for Pat {
    fn cast(origin: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        Some(match origin.kind() {
            SyntaxKind::IdentPat => Pat::IdentPat(IdentPat(origin)),
            SyntaxKind::WildcardPat => Pat::WildcardPat(WildcardPat(origin)),
            _ => return None,
        })
    }

    fn origin(&self) -> &SyntaxNode {
        match self {
            Pat::IdentPat(p) => p.origin(),
            Pat::WildcardPat(p) => p.origin(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct IdentPat(SyntaxNode);

impl IdentPat {
    pub fn ident(&self) -> Option<SyntaxToken> {
        token(&self.0, Token![ident])
    }
}

impl AstNode for IdentPat {
    fn cast(origin: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        if origin.kind() == SyntaxKind::IdentPat {
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
pub struct WildcardPat(SyntaxNode);

impl WildcardPat {
    pub fn underscore(&self) -> Option<SyntaxToken> {
        token(&self.0, Token![_])
    }
}

impl AstNode for WildcardPat {
    fn cast(origin: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        if origin.kind() == SyntaxKind::IdentPat {
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
pub enum Expr {
    CodeBlock(CodeBlock),
    Literal(Literal),
    BinaryExpr(BinaryExpr),
    UnaryExpr(UnaryExpr),
    ParenExpr(ParenExpr),
    WhileExpr(WhileExpr),
    ForExpr(ForExpr),
    IfExpr(IfExpr),
    BreakExpr(BreakExpr),
    ContinueExpr(ContinueExpr),
    ArrayExpr(ArrayExpr),
    LetBinding(LetBinding),
    NameRef(NameRef),
}

impl AstNode for Expr {
    fn cast(origin: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        Some(match origin.kind() {
            SyntaxKind::CodeBlock => Expr::CodeBlock(CodeBlock(origin)),
            SyntaxKind::Literal => Expr::Literal(Literal(origin)),
            SyntaxKind::BinaryExpr => Expr::BinaryExpr(BinaryExpr(origin)),
            SyntaxKind::UnaryExpr => Expr::UnaryExpr(UnaryExpr(origin)),
            SyntaxKind::ParenExpr => Expr::ParenExpr(ParenExpr(origin)),
            SyntaxKind::WhileExpr => Expr::WhileExpr(WhileExpr(origin)),
            SyntaxKind::ForExpr => Expr::ForExpr(ForExpr(origin)),
            SyntaxKind::IfExpr => Expr::IfExpr(IfExpr(origin)),
            SyntaxKind::BreakExpr => Expr::BreakExpr(BreakExpr(origin)),
            SyntaxKind::ContinueExpr => Expr::ContinueExpr(ContinueExpr(origin)),
            SyntaxKind::ArrayExpr => Expr::ArrayExpr(ArrayExpr(origin)),
            SyntaxKind::LetBinding => Expr::LetBinding(LetBinding(origin)),
            SyntaxKind::NameRef => Expr::NameRef(NameRef(origin)),
            _ => return None,
        })
    }

    fn origin(&self) -> &SyntaxNode {
        match self {
            Expr::CodeBlock(e) => e.origin(),
            Expr::Literal(e) => e.origin(),
            Expr::BinaryExpr(e) => e.origin(),
            Expr::UnaryExpr(e) => e.origin(),
            Expr::ParenExpr(e) => e.origin(),
            Expr::WhileExpr(e) => e.origin(),
            Expr::ForExpr(e) => e.origin(),
            Expr::IfExpr(e) => e.origin(),
            Expr::BreakExpr(e) => e.origin(),
            Expr::ContinueExpr(e) => e.origin(),
            Expr::ArrayExpr(e) => e.origin(),
            Expr::LetBinding(e) => e.origin(),
            Expr::NameRef(e) => e.origin(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct CodeBlock(SyntaxNode);

impl CodeBlock {
    pub fn open_brace(&self) -> Option<SyntaxToken> {
        token(&self.0, Token!['{'])
    }

    pub fn body(&self) -> ChildIter<Expr> {
        children(&self.0)
    }

    pub fn close_brace(&self) -> Option<SyntaxToken> {
        token(&self.0, Token!['}'])
    }
}

impl AstNode for CodeBlock {
    fn cast(origin: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        if origin.kind() == SyntaxKind::CodeBlock {
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
    pub fn new(parent: &SyntaxNode) -> Self {
        Self {
            inner: parent.children(),
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

fn children<N: AstNode>(parent: &SyntaxNode) -> ChildIter<N> {
    ChildIter::new(parent)
}

fn child<N: AstNode>(parent: &SyntaxNode) -> Option<N> {
    parent.children().find_map(N::cast)
}

fn token(parent: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxToken> {
    parent
        .children_with_tokens()
        .filter_map(|it| it.into_token())
        .find(|it| it.kind() == kind)
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
pub struct UnaryExpr(SyntaxNode);

impl UnaryExpr {
    pub fn op(&self) -> Option<SyntaxToken> {
        self.0.first_child_or_token()?.into_token()
    }

    pub fn operand(&self) -> Option<Expr> {
        child(&self.0)
    }
}

impl AstNode for UnaryExpr {
    fn cast(origin: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        if origin.kind() == SyntaxKind::UnaryExpr {
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
pub struct ParenExpr(SyntaxNode);

impl ParenExpr {
    pub fn open_paren(&self) -> Option<SyntaxToken> {
        token(&self.0, Token!['('])
    }

    pub fn expr(&self) -> Option<Expr> {
        child(&self.0)
    }

    pub fn close_paren(&self) -> Option<SyntaxToken> {
        token(&self.0, Token![')'])
    }
}

impl AstNode for ParenExpr {
    fn cast(origin: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        if origin.kind() == SyntaxKind::ParenExpr {
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
pub struct WhileExpr(SyntaxNode);

impl WhileExpr {
    pub fn token(&self) -> SyntaxToken {
        self.0
            .first_child_or_token()
            .and_then(|it| it.into_token())
            .unwrap()
    }

    pub fn condition(&self) -> Option<Expr> {
        child(&self.0)
    }

    pub fn body(&self) -> Option<CodeBlock> {
        child(&self.0)
    }
}

impl AstNode for WhileExpr {
    fn cast(origin: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        if origin.kind() == SyntaxKind::WhileExpr {
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
pub struct ForExpr(SyntaxNode);

impl ForExpr {
    pub fn token(&self) -> SyntaxToken {
        self.0
            .first_child_or_token()
            .and_then(|it| it.into_token())
            .unwrap()
    }

    pub fn pat(&self) -> Option<Pat> {
        child(&self.0)
    }

    pub fn in_token(&self) -> Option<SyntaxToken> {
        token(&self.0, Token![in])
    }

    pub fn collection(&self) -> Option<Expr> {
        child(&self.0)
    }

    pub fn body(&self) -> Option<CodeBlock> {
        child(&self.0)
    }
}

impl AstNode for ForExpr {
    fn cast(origin: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        if origin.kind() == SyntaxKind::ForExpr {
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
pub struct IfExpr(SyntaxNode);

impl IfExpr {
    pub fn if_token(&self) -> Option<SyntaxToken> {
        token(&self.0, Token![if])
    }

    pub fn else_token(&self) -> Option<SyntaxToken> {
        token(&self.0, Token![else])
    }
}

impl AstNode for IfExpr {
    fn cast(origin: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        if origin.kind() == SyntaxKind::IfExpr {
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
pub struct BreakExpr(SyntaxNode);

impl BreakExpr {
    pub fn token(&self) -> Option<SyntaxToken> {
        token(&self.0, Token![break])
    }
}

impl AstNode for BreakExpr {
    fn cast(origin: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        if origin.kind() == SyntaxKind::BreakExpr {
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
pub struct ContinueExpr(SyntaxNode);

impl ContinueExpr {
    pub fn token(&self) -> Option<SyntaxToken> {
        token(&self.0, Token![continue])
    }
}

impl AstNode for ContinueExpr {
    fn cast(origin: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        if origin.kind() == SyntaxKind::ContinueExpr {
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
pub struct ArrayExpr(SyntaxNode);

impl ArrayExpr {
    pub fn open_bracket(&self) -> Option<SyntaxToken> {
        token(&self.0, Token!['['])
    }

    pub fn expr(&self) -> Option<Expr> {
        child(&self.0)
    }

    pub fn close(&self) -> Option<SyntaxToken> {
        token(&self.0, Token![']'])
    }
}

impl AstNode for ArrayExpr {
    fn cast(origin: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        if origin.kind() == SyntaxKind::ArrayExpr {
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
pub struct LetBinding(SyntaxNode);

impl LetBinding {
    pub fn let_token(&self) -> Option<SyntaxToken> {
        token(&self.0, Token![let])
    }

    pub fn pat(&self) -> Option<Pat> {
        child(&self.0)
    }

    pub fn eq_token(&self) -> Option<SyntaxToken> {
        token(&self.0, Token![=])
    }

    pub fn init(&self) -> Option<Expr> {
        child(&self.0)
    }
}

impl AstNode for LetBinding {
    fn cast(origin: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        if origin.kind() == SyntaxKind::LetBinding {
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
pub struct NameRef(SyntaxNode);

impl NameRef {
    pub fn ident(&self) -> Option<SyntaxToken> {
        token(&self.0, Token![ident])
    }
}

impl AstNode for NameRef {
    fn cast(origin: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        if origin.kind() == SyntaxKind::NameRef {
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
        if origin.kind() == Token![ident] {
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
        if origin.kind() == Token![int] {
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
        if origin.kind() == Token![float] {
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
        if origin.kind() == Token![string] {
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
pub struct Comment(SyntaxToken);

impl AstToken for Comment {
    fn cast(origin: SyntaxToken) -> Option<Self>
    where
        Self: Sized,
    {
        if origin.kind() == Token![comment] {
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
pub struct Whitespace(SyntaxToken);

impl AstToken for Whitespace {
    fn cast(origin: SyntaxToken) -> Option<Self>
    where
        Self: Sized,
    {
        if origin.kind() == Token![whitespace] {
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
    fn code_block() {
        let (root, _) = crate::parser::parse("{1 + 2}");
        let block = expr::<CodeBlock>(&root);
        let expr = stringify_expr(&block.body().next().unwrap());
        assert_eq!("{", block.open_brace().unwrap().text());
        assert_eq!("1 + 2", expr);
        assert_eq!("}", block.close_brace().unwrap().text());
    }

    #[test]
    fn literal() {
        let (root, _) = crate::parser::parse(r#"let foo = "bar""#);
        let lit = expr::<Literal>(&root);
        assert_eq!(r#""bar""#, lit.token().text());
    }

    #[test]
    fn binary_expr() {
        let (root, _) = crate::parser::parse("1 + 2");
        let binary = expr::<BinaryExpr>(&root);
        let lhs = stringify_expr(&binary.lhs().unwrap());
        let rhs = stringify_expr(&binary.rhs().unwrap());
        assert_eq!("1", lhs);
        assert_eq!("+", binary.op().unwrap().text());
        assert_eq!("2", rhs);
    }

    #[test]
    fn unary_expr() {
        let (root, _) = crate::parser::parse("-2");
        let unary = expr::<UnaryExpr>(&root);
        let operand = stringify_expr(&unary.operand().unwrap());
        assert_eq!("-", unary.op().unwrap().text());
        assert_eq!("2", operand);
    }

    #[test]
    fn paren_expr() {
        let (root, _) = crate::parser::parse("(1 + 2)");
        let paren_expr = expr::<ParenExpr>(&root);
        let expr = stringify_expr(&paren_expr.expr().unwrap());
        assert_eq!("(", paren_expr.open_paren().unwrap().text());
        assert_eq!("1 + 2", expr);
        assert_eq!(")", paren_expr.close_paren().unwrap().text());
    }

    #[test]
    fn while_expr() {
        let (root, _) = crate::parser::parse("while true {}");
        let while_expr = expr::<WhileExpr>(&root);
        let condition = stringify_expr(&while_expr.condition().unwrap());
        let has_body = while_expr.body().is_some();
        assert_eq!("while", while_expr.token().text());
        assert_eq!("true", condition);
        assert_eq!(true, has_body);
    }

    fn expr<N: AstNode>(root: &SyntaxNode) -> N {
        root.descendants().find_map(N::cast).unwrap()
    }

    fn stringify_expr(expr: &Expr) -> std::string::String {
        expr.origin().text().to_string()
    }
}
