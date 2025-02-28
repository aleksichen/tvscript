use lexer::lexer::LexedToken;
use lexer::lexer::Token;
use lexer::lexer::TvLexer as Lexer;
use lexer::span::Span;

use crate::ast::Ast;
use crate::ast::AstIndex;
use crate::error::Error;
use crate::error::Result;
use crate::node::AstVec;
use crate::node::Node;

trait RecursiveDescent {
    /// 解析一个程序。
    fn parse_program(&mut self) -> Result<AstIndex>;

    /// 解析一个语句。
    fn parse_statement(&mut self) -> Result<Option<AstIndex>>;

    /// 解析变量声明语句
    fn parse_variable_declaration(&mut self) -> Result<Option<AstIndex>>;

    /// 解析赋值语句
    fn parse_assignment(&mut self) -> Result<Option<AstIndex>>;

    /// 解析表达式
    fn parse_expression(&mut self) -> Result<Option<AstIndex>>;
}

pub trait ParserApi {
    /// 预读第n个token（不消费）
    fn peek(&mut self, n: usize) -> Option<&LexedToken>;
    
    /// 消费当前token并返回
    fn consume_token(&mut self) -> Option<LexedToken>;
    
    /// 获取token对应的原始字符串
    fn token_source(&self, token: &LexedToken) -> &str;   
}

// Returned by Parser::peek_token_with_context()
#[derive(Debug)]
struct PeekInfo {
    token: Token,
    peek_count: usize,
    info: LexedToken,
}

pub struct Parser<'source> {
    source: &'source str,
    ast: Ast,
    lexer: Lexer<'source>,
    current_token: LexedToken,
    current_line: u32,
}

impl<'source> Parser<'source> {
    pub fn parse(source: &'source str) -> Ast {
        let parser = Parser {
            source,
            ast: Ast::with_capacity(source.len() / 4),
            lexer: Lexer::new(source),
            current_token: LexedToken::default(),
            current_line: 0,
        };

        // 主入口
        // parser.parse_program();
        parser.ast
    }

    // 消耗空白字符、注释和换行符，直到下一个 token
    fn consume_until_token(&mut self) {
        while let Some(peeked) = self.lexer.peek(0) {
            match peeked.token {
                Token::Whitespace | Token::NewLine | Token::Comment => {
                    self.consume_token();
                }
                _ => {}
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
}

impl<'a> ParserApi for Parser<'a> {
    fn peek(&mut self, n: usize) -> Option<&LexedToken> {
        // 直接委托给 TvLexer 的缓冲队列
        self.lexer.peek(n)
    }
    fn consume_token(&mut self) -> Option<LexedToken> {
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
    fn token_source(&self, token: &LexedToken) -> &str {
        // 通过字节范围直接访问源字符串
        let bytes = &self.lexer.source().as_bytes()[token.source_bytes.clone()];
        
        // 安全转换（已验证的 UTF-8 输入）
        unsafe { std::str::from_utf8_unchecked(bytes) }
    }
}

impl<'source> RecursiveDescent for Parser<'source> {
    /// 程序主入口
    fn parse_program(&mut self) -> Result<AstIndex> {
        let mut statements = AstVec::new();
        let start_span = self.current_span();

        while let Some(peek_info) = self.peek_valid_token() {
            match peek_info.token {
                Token::EOF => break, // 到达文件末尾，结束解析
                _ => {
                    if let Ok(Some(statement)) = self.parse_statement() {
                        statements.push(statement);
                    } else {
                        // 如果解析语句失败，可以添加错误处理逻辑
                        break;
                    }
                }
            }
        }

        let result = self.push_node_with_start_span(Node::Program { statements }, start_span)?;

        Ok(result)
    }

    fn parse_statement(&mut self) -> Result<Option<AstIndex>> {
        if let Some(token) = self.peek_token() {
            match token {
                Token::Var => {
                    // 变量声明语句
                    self.parse_variable_declaration()
                }
                Token::Identifier => {
                    // 尝试解析赋值语句或表达式语句
                    match self.parse_assignment() {
                        Ok(assignment) => return Ok(assignment),
                        Err(_) => self.parse_expression(), // 如果赋值解析失败，尝试解析表达式
                    }
                }
                _ => {
                    // 表达式语句
                    self.parse_expression()
                }
            }
        } else {
            // 如果没有 token，返回一个错误
            Err(Error::new(
                crate::error::ErrorKind::SyntaxError(crate::error::SyntaxError::UnexpectedToken),
                self.current_span(),
            ))
        }
    }

    fn parse_variable_declaration(&mut self) -> Result<Option<AstIndex>> {
        todo!()
        // // 变量声明语句解析，例如：var a = 1 + 1;
        // let start_span = self.current_span();

        // // 消耗 'var' 关键字
        // self.consume_token();

        // // 解析标识符
        // let ident_info = self.peek_valid_token();
        // let ident_index = match ident_info {
        //     Some(info) => {
        //         if let Token::Identifier = info.token {
        //             self.consume_token();
        //             self.push_node_with_start_span(Node::Identifier, info.info.span)?
        //         } else {
        //             return Err(Error::new(
        //                 crate::error::ErrorKind::SyntaxError(
        //                     crate::error::SyntaxError::ExpectedAssignmentTarget,
        //                 ),
        //                 self.current_span(),
        //             ));
        //         }
        //     }
        //     None => {
        //         return Err(Error::new(
        //             crate::error::ErrorKind::SyntaxError(
        //                 crate::error::SyntaxError::ExpectedAssignmentTarget,
        //             ),
        //             self.current_span(),
        //         ));
        //     }
        // };
        // // 解析表达式
        // let init_index = self.parse_expression()?;

        // // 创建 VariableDecl 节点
        // let variable_decl_node = Node::VariableDecl {
        //     ident: ident_index,
        //     init: init_index,
        // };

        // self.push_node_with_start_span(variable_decl_node, start_span)
    }

    fn parse_assignment(&mut self) -> Result<Option<AstIndex>> {
        todo!()
    }

    fn parse_expression(&mut self) -> Result<Option<AstIndex>> {
        // 此处接入pratt算法
        todo!()
    }
}
