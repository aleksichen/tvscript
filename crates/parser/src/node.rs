use crate::ast::AstIndex;
use smallvec::SmallVec;

pub type AstVec<T> = SmallVec<[T; 4]>;

pub use smallvec::smallvec as astvec;

/// Nodes refer to each other via [`AstIndex`], see [`AstNode`](crate::AstNode).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Node {
    // 根节点
    Program {
        statements: AstVec<AstIndex>,
    },

    // 变量声明节点
    VariableDecl {
        ident: AstIndex,
        init: AstIndex,
    },

    /// 标识符节点
    Identifier {
        name: String
    },

    /// 一元运算表达式
    UnaryExpr {
        op: UnaryOp,
        expr: AstIndex,
    },

    /// 二元运算表达式
    BinaryExpr {
        op: AstIndex,
        left: AstIndex,
        right: AstIndex,
    },

    Assignment {
        target: AstIndex,     // 左值
        expression: AstIndex, // 右值表达式
    },
}

/// 一元表达式操作符
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum UnaryOp {
    Negate,     // - (数值取反)
    LogicalNot, // ! (逻辑非)
    BitwiseNot, // ~ (按位取反)
}

/// 二元表达式操作符
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    RemainderAssign,
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
}
