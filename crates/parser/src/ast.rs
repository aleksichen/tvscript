use std::{fmt, num::TryFromIntError};

use lexer::span::Span;

use crate::{
    error::{Error, InternalError, Result},
    node::Node,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct AstIndex(u32);

impl From<AstIndex> for u32 {
    fn from(value: AstIndex) -> Self {
        value.0
    }
}

impl From<AstIndex> for usize {
    fn from(value: AstIndex) -> Self {
        value.0 as usize
    }
}

impl From<u32> for AstIndex {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl From<&u32> for AstIndex {
    fn from(value: &u32) -> Self {
        Self(*value)
    }
}

impl TryFrom<usize> for AstIndex {
    type Error = TryFromIntError;

    fn try_from(value: usize) -> std::result::Result<Self, Self::Error> {
        Ok(Self(u32::try_from(value)?))
    }
}

impl fmt::Display for AstIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// 关联了span的ast node
#[derive(Clone, Debug)]
pub struct AstNode {
    /// The node itself
    pub node: Node,
    /// The index of the node's corresponding [Span]
    ///
    /// The span is stored in the [Ast], and retrieved via [Ast::span].
    pub span: AstIndex,
}

/// pine script 抽象语法树
/// 由parser生成, 被compile消费.
#[derive(Debug, Clone)]
pub struct Ast {
    nodes: Vec<AstNode>,
    spans: Vec<Span>,
}

impl Ast {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            nodes: Vec::with_capacity(capacity),
            spans: Vec::with_capacity(capacity),
        }
    }

    pub fn push(&mut self, node: Node, span: Span) -> Result<AstIndex> {
        // We could potentially achieve some compression by
        // using a set for the spans, for now a Vec will do.
        self.spans.push(span);
        let span_index = AstIndex::try_from(self.spans.len() - 1)
            .map_err(|_| Error::new(InternalError::AstCapacityOverflow.into(), span))?;

        self.nodes.push(AstNode {
            node,
            span: span_index,
        });
        AstIndex::try_from(self.nodes.len() - 1)
            .map_err(|_| Error::new(InternalError::AstCapacityOverflow.into(), span))
    }
}
