use lexer::lexer::LexedToken;
use lexer::lexer::Token;
use lexer::lexer::TvLexer as Lexer;
use lexer::span::Span;

use crate::ast::Ast;
use crate::ast::AstIndex;
use crate::core::ParserCore;
use crate::core::ParserCoreBuilder;
use crate::error::Error;
use crate::error::Result;
use crate::node::AstVec;
use crate::node::Node;
use crate::pratt::create_pratt_parser;
use crate::pratt::Expr;
use crate::pratt::PrattParser;

// 
pub struct TvParser<'a> {
    pub pratt: PrattParser<'a>,
    pub core: ParserCore<'a>,
}

impl <'a>TvParser<'a> {


    pub fn parse () {}
}

// trait RecursiveDescent {
//     /// 解析一个程序。
//     fn parse_program(&mut self) -> Result<AstIndex>;

//     /// 解析一个语句。
//     fn parse_statement(&mut self) -> Result<Option<AstIndex>>;

//     /// 解析变量声明语句
//     fn parse_variable_declaration(&mut self) -> Result<Option<AstIndex>>;

//     /// 解析赋值语句
//     fn parse_assignment(&mut self) -> Result<Option<AstIndex>>;

//     /// 解析表达式
//     fn parse_expression(&mut self) -> Result<Option<AstIndex>>;
// }

// impl<'source> RecursiveDescent for Parser<'source> {
//     /// 程序主入口
//     fn parse_program(&mut self) -> Result<AstIndex> {
//         let mut statements = AstVec::new();
//         let start_span = self.current_span();

//         while let Some(peek_info) = self.peek_valid_token() {
//             match peek_info.token {
//                 Token::EOF => break, // 到达文件末尾，结束解析
//                 _ => {
//                     if let Ok(Some(statement)) = self.parse_statement() {
//                         statements.push(statement);
//                     } else {
//                         // 如果解析语句失败，可以添加错误处理逻辑
//                         break;
//                     }
//                 }
//             }
//         }

//         let result = self.push_node_with_start_span(Node::Program { statements }, start_span)?;

//         Ok(result)
//     }

//     fn parse_statement(&mut self) -> Result<Option<AstIndex>> {
//         if let Some(token) = self.peek_token() {
//             match token {
//                 Token::Var => {
//                     // 变量声明语句
//                     self.parse_variable_declaration()
//                 }
//                 Token::Identifier => {
//                     // 尝试解析赋值语句或表达式语句
//                     match self.parse_assignment() {
//                         Ok(assignment) => return Ok(assignment),
//                         Err(_) => self.parse_expression(), // 如果赋值解析失败，尝试解析表达式
//                     }
//                 }
//                 _ => {
//                     // 表达式语句
//                     self.parse_expression()
//                 }
//             }
//         } else {
//             // 如果没有 token，返回一个错误
//             Err(Error::new(
//                 crate::error::ErrorKind::SyntaxError(crate::error::SyntaxError::UnexpectedToken),
//                 self.current_span(),
//             ))
//         }
//     }

//     fn parse_variable_declaration(&mut self) -> Result<Option<AstIndex>> {
//         todo!()
//         // // 变量声明语句解析，例如：var a = 1 + 1;
//         // let start_span = self.current_span();

//         // // 消耗 'var' 关键字
//         // self.consume_token();

//         // // 解析标识符
//         // let ident_info = self.peek_valid_token();
//         // let ident_index = match ident_info {
//         //     Some(info) => {
//         //         if let Token::Identifier = info.token {
//         //             self.consume_token();
//         //             self.push_node_with_start_span(Node::Identifier, info.info.span)?
//         //         } else {
//         //             return Err(Error::new(
//         //                 crate::error::ErrorKind::SyntaxError(
//         //                     crate::error::SyntaxError::ExpectedAssignmentTarget,
//         //                 ),
//         //                 self.current_span(),
//         //             ));
//         //         }
//         //     }
//         //     None => {
//         //         return Err(Error::new(
//         //             crate::error::ErrorKind::SyntaxError(
//         //                 crate::error::SyntaxError::ExpectedAssignmentTarget,
//         //             ),
//         //             self.current_span(),
//         //         ));
//         //     }
//         // };
//         // // 解析表达式
//         // let init_index = self.parse_expression()?;

//         // // 创建 VariableDecl 节点
//         // let variable_decl_node = Node::VariableDecl {
//         //     ident: ident_index,
//         //     init: init_index,
//         // };

//         // self.push_node_with_start_span(variable_decl_node, start_span)
//     }

//     fn parse_assignment(&mut self) -> Result<Option<AstIndex>> {
//         todo!()
//     }

//     fn parse_expression(&mut self) -> Result<Option<AstIndex>> {
//         // 此处接入pratt算法
//         todo!()
//     }
// }

// 定义可测试的 trait 接口
pub trait ExpressionParser {
    fn parse_expression(
        &self,
        core: &mut ParserCore,
        precedence: u8
    ) -> Result<Expr>;
}

pub struct RecursiveDescent<P: ExpressionParser> {
    expr_parser: P,
}

impl <'a> ExpressionParser for PrattParser <'a>{
    fn parse_expression(
        &self,
        core: &mut ParserCore,
        precedence: u8
    ) -> Result<Expr> {
        todo!()
    }
}
