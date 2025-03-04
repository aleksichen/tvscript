use lexer::lexer::LexedToken;
use lexer::lexer::Token;
use lexer::lexer::TvLexer as Lexer;
use lexer::span::Span;

use crate::ast::Ast;
use crate::ast::AstIndex;
use crate::error::Result;
use crate::node::Node;

#[derive(Debug)]
pub struct PeekInfo {
    pub token: Token,
    pub peek_count: usize,
    pub info: LexedToken,
}

#[derive(Debug)]
pub struct ParserCore<'source> {
    pub source: &'source str,
    pub ast: Ast,
    pub lexer: Lexer<'source>,
    pub current_token: LexedToken,
    pub current_line: u32,
}

pub struct ParserCoreBuilder;

impl ParserCoreBuilder {
    pub fn build(source: &str) -> ParserCore {
        ParserCore {
            source,
            ast: Ast::with_capacity(source.len() / 4),
            lexer: Lexer::new(source),
            current_token: LexedToken::default(),
            current_line: 0,
        }
    }
}

impl<'source> ParserCore<'source> {
    /// 消耗空白字符、注释和换行符，直到下一个有效 token
    pub fn consume_until_token(&mut self) {
        loop {
            // 检查下一个 token 的类型
            match self.lexer.peek(0) {
                Some(peeked) => match peeked.token {
                    Token::Whitespace | Token::NewLine | Token::Comment => {
                        self.consume_token(); // 消耗无效 token
                    }
                    _ => {
                        break;
                    },
                },
                None => break, // 没有更多 token 时终止
            }
        }
    }

    // 返回有效的token, 过滤注释, 空格, 换行符
    fn peek_valid_token(&mut self) -> Option<PeekInfo> {
        let mut peek_count = 0;

        while let Some(peeked) = self.lexer.peek(peek_count) {
            match peeked.token {
                Token::Whitespace | Token::NewLine | Token::Comment => {}
                token => {
                    let result = Some(PeekInfo {
                        token,
                        peek_count,
                        info: peeked.clone(),
                    });
                    return result;
                }
            }

            peek_count += 1;
        }
        None
    }

    fn peek_token(&mut self) -> Option<Token> {
        self.peek_token_n(0)
    }

    fn peek_token_n(&mut self, n: usize) -> Option<Token> {
        self.lexer.peek(n).map(|peeked| peeked.token)
    }

    fn current_span(&self) -> Span {
        self.current_token.span
    }

    fn push_node_with_start_span(&mut self, node: Node, start_span: Span) -> Result<AstIndex> {
        self.push_node_with_span(node, self.span_with_start(start_span))
    }

    fn push_node_with_span(&mut self, node: Node, span: Span) -> Result<AstIndex> {
        self.ast.push(node, span)
    }

    fn span_with_start(&self, start_span: Span) -> Span {
        Span {
            start: start_span.start,
            end: self.current_span().end,
        }
    }

    pub fn peek(&mut self, n: usize) -> Option<LexedToken> {
        // 直接委托给 TvLexer 的缓冲队列，并克隆结果
        self.lexer.peek(n).cloned()
    }

    // 消费一个token
    pub fn consume_token(&mut self) -> Option<LexedToken> {
        if let Some(next) = self.lexer.next() {
            self.current_token = next.clone();

            if self.current_token.token == Token::NewLine {
                self.current_line += 1;
            }

            Some(next)
        } else {
            None
        }
    }

    /// 实现原始字符串访问（零拷贝优化）
    pub fn token_source(&self, token: &LexedToken) -> &str {
        // 通过字节范围直接访问源字符串
        let bytes = &self.lexer.source().as_bytes()[token.source_bytes.clone()];

        // 安全转换（已验证的 UTF-8 输入）
        unsafe { std::str::from_utf8_unchecked(bytes) }
    }
}
