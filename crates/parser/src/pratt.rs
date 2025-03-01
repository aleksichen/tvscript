use std::collections::HashMap;
use std::rc::Rc;

use lexer::lexer::{LexedToken, Token};

use crate::parser::ParserApi;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
/// 运算符优先级枚举
pub enum PrecedenceLevel {
    /// 最低优先级
    Lowest = 0,
    /// 赋值运算符优先级
    Assignment = 1,
    /// 加法和减法运算符优先级
    Sum = 10,
    /// 乘法和除法运算符优先级
    Product = 20,
    /// 指数运算符优先级
    Exponent = 30,
    /// 前缀运算符优先级，如一元加减
    Prefix = 40,
    /// 函数调用运算符优先级
    Call = 50,
}

impl PrecedenceLevel {
    /// 获取优先级的值（数值越大，优先级越高）
    fn value(self) -> u32 {
        self as u32
    }

    /// 获取前一个优先级级别（用于处理右结合运算符）
    fn predecessor(self) -> Self {
        match self {
            Self::Exponent => Self::Product,
            Self::Product => Self::Sum,
            Self::Sum => Self::Assignment,
            _ => Self::Lowest,
        }
    }
}

/// 表达式枚举，表示解析后的表达式树
#[derive(Debug, PartialEq)]
enum Expr {
    /// 整数字面量
    Integer(i64),
    /// 二元运算表达式
    BinaryOp {
        /// 运算符 Token
        op: Token,
        /// 左操作数表达式
        left: Box<Expr>,
        /// 右操作数表达式
        right: Box<Expr>,
    },
    /// 函数调用表达式
    Call {
        /// 被调用的函数表达式（可以是标识符或复杂表达式）
        func: Box<Expr>,
        /// 参数列表
        args: Vec<Expr>,
    },
    Identifier(String),
    /// 成员访问表达式 (如 obj.property)
    MemberAccess {
        object: Box<Expr>, // 左侧对象表达式
        member: String,    // 成员名称
    },
    // 三元表达式
    Ternary {
        cond_expr: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    // 数组字面量
    ArrayLiteral(Vec<Expr>),
    // 数组下标访问
    IndexAccess {
        array: Box<Expr>,
        index: Box<Expr>,
    },
}

/// 前缀解析子句 Trait，定义前缀运算符的解析行为
trait PrefixParselet {
    /// 解析前缀表达式
    ///
    /// # 参数
    /// * `parser` - PrattParser 的可变引用，用于递归解析子表达式
    /// * `token` - 当前正在解析的 Token (前缀 Token)
    fn parse(&self, parser: &mut PrattParser, token: LexedToken) -> Expr;
}

/// 中缀解析子句 Trait，定义中缀运算符的解析行为
trait InfixParselet {
    /// 解析中缀表达式
    ///
    /// # 参数
    /// * `parser` - PrattParser 的可变引用，用于递归解析右操作数
    /// * `left` - 左操作数表达式，已经由前缀或更低优先级的运算符解析完成
    /// * `token` - 当前正在解析的 Token (中缀运算符 Token)
    fn parse(&self, parser: &mut PrattParser, left: Expr, token: LexedToken) -> Expr;
    /// 获取当前中缀运算符的优先级
    fn precedence(&self) -> PrecedenceLevel;
}

/// 数字字面量解析子句
struct NumberParselet;

impl PrefixParselet for NumberParselet {
    /// 解析数字字面量
    fn parse(&self, _parser: &mut PrattParser, token: LexedToken) -> Expr {
        // 目前只处理 Integer Token，实际应用中需要根据 Token 类型和值进行更精细的处理
        if let Token::Integer = token.token {
            Expr::Integer(1) // 示例代码，简单返回 Integer(1)
        } else {
            panic!("Unexpected token") // 遇到非 Integer Token，panic
        }
    }
}

// 数组字面量解析器
struct ArrayParselet;
impl PrefixParselet for ArrayParselet {
    fn parse(&self, parser: &mut PrattParser, _: LexedToken) -> Expr {
        let mut elements = vec![];

        while parser.parser.peek(0).map(|t| t.token) != Some(Token::SquareClose) {
            elements.push(parser.parse_expression(PrecedenceLevel::Lowest));
            if parser.parser.peek(0).map(|t| t.token) == Some(Token::Comma) {
                parser.parser.consume_token();
            }
        }
        parser.parser.consume_token(); // 消耗 ]
        Expr::ArrayLiteral(elements)
    }
}

/// 二元运算符解析子句
struct BinaryOpParselet {
    /// 运算符优先级
    precedence: PrecedenceLevel,
    /// 是否右结合
    is_right_assoc: bool,
}

impl InfixParselet for BinaryOpParselet {
    /// 解析二元运算表达式
    fn parse(&self, parser: &mut PrattParser, left: Expr, token: LexedToken) -> Expr {
        // 根据运算符是否右结合，确定解析右操作数时的最小优先级
        let actual_prec = if self.is_right_assoc {
            self.precedence.predecessor() // 右结合运算符，右操作数优先级需要降低一级
        } else {
            self.precedence // 左结合运算符，右操作数优先级保持当前运算符优先级
        };

        // 递归调用 parse_expression 解析右操作数
        let right = parser.parse_expression(actual_prec);
        Expr::BinaryOp {
            op: token.token,
            left: Box::new(left),   // 将左操作数放入 Box
            right: Box::new(right), // 将右操作数放入 Box
        }
    }

    /// 返回二元运算符的优先级
    fn precedence(&self) -> PrecedenceLevel {
        self.precedence
    }
}

struct GroupParselet;
impl PrefixParselet for GroupParselet {
    fn parse(&self, parser: &mut PrattParser, _: LexedToken) -> Expr {
        let expr = parser.parse_expression(PrecedenceLevel::Lowest);
        assert_eq!(
            parser.parser.consume_token().unwrap().token,
            Token::RoundClose
        );
        expr
    }
}

struct CallParselet {
    precedence: PrecedenceLevel,
}
impl InfixParselet for CallParselet {
    fn parse(&self, parser: &mut PrattParser, left: Expr, _: LexedToken) -> Expr {
        let mut args = vec![];
        while parser.parser.peek(0).map(|t| t.token) != Some(Token::RoundClose) {
            args.push(parser.parse_expression(PrecedenceLevel::Lowest));
            if parser.parser.peek(0).map(|t| t.token) == Some(Token::Comma) {
                parser.parser.consume_token();
            }
        }
        parser.parser.consume_token(); // 消耗右括号
        Expr::Call {
            func: Box::new(left),
            args,
        }
    }

    fn precedence(&self) -> PrecedenceLevel {
        self.precedence
    }
}

struct IdentifierParselet;

impl PrefixParselet for IdentifierParselet {
    fn parse(&self, _: &mut PrattParser, token: LexedToken) -> Expr {
        if let Token::Identifier = &token.token {
            Expr::Identifier("id".to_string())
        } else {
            panic!("Unexpected token for identifier")
        }
    }
}

struct DotParselet;

impl InfixParselet for DotParselet {
    fn parse(&self, parser: &mut PrattParser, left: Expr, token: LexedToken) -> Expr {
        let member_token = parser
            .parser
            .consume_token()
            .expect("Expected identifier after '.'");
        let member = if let Token::Identifier = member_token.token {
            "id".to_string()
        } else {
            panic!("Expected identifier after '.'");
        };
        Expr::MemberAccess {
            object: Box::new(left),
            member,
        }
    }

    fn precedence(&self) -> PrecedenceLevel {
        PrecedenceLevel::Call // 与函数调用同级
    }
}

// 三元条件表达式解析器
struct TernaryParselet;
impl InfixParselet for TernaryParselet {
    fn parse(&self, parser: &mut PrattParser, cond: Expr, _: LexedToken) -> Expr {
        // 解析then语句
        let then_expr = parser.parse_expression(PrecedenceLevel::Lowest);
        parser.parser.consume_token(); // 跳过 :
                                       // 解析else语句
        let else_expr = parser.parse_expression(PrecedenceLevel::Assignment);
        Expr::Ternary {
            cond_expr: Box::new(cond),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
        }
    }

    fn precedence(&self) -> PrecedenceLevel {
        PrecedenceLevel::Assignment // 优先级低于逻辑运算
    }
}

// 下标访问解析器
struct IndexParselet;
impl InfixParselet for IndexParselet {
    fn parse(&self, parser: &mut PrattParser, left: Expr, _: LexedToken) -> Expr {
        let index = parser.parse_expression(PrecedenceLevel::Lowest);
        parser.parser.consume_token(); // 消耗 ]
        Expr::IndexAccess {
            array: Box::new(left),
            index: Box::new(index),
        }
    }
    fn precedence(&self) -> PrecedenceLevel {
        PrecedenceLevel::Call
    }
}

/// Pratt Parser 结构体
pub struct PrattParser<'a> {
    /// 前缀解析函数映射表，key 是 Token 类型，value 是实现了 PrefixParselet trait 的解析器
    prefix_parselets: HashMap<Token, Rc<dyn PrefixParselet>>,
    /// 中缀解析函数映射表，key 是 Token 类型，value 是实现了 InfixParselet trait 的解析器
    infix_parselets: HashMap<Token, Rc<dyn InfixParselet>>,
    parser: &'a mut dyn ParserApi,
}

impl<'a> PrattParser<'a> {
    /// 创建 PrattParser 实例
    fn new(parser: &'a mut dyn ParserApi) -> Self {
        Self {
            prefix_parselets: HashMap::new(),
            infix_parselets: HashMap::new(),
            parser,
        }
    }

    /// 解析表达式，入口方法
    ///
    /// # 参数
    /// * `min_precedence` - 最小优先级，用于控制运算符的结合性
    fn parse_expression(&mut self, min_precedence: PrecedenceLevel) -> Expr {
        // 先解析前缀表达式
        let mut left = self.parse_prefix();

        // 确保只有在存在下一个token且优先级足够高时才继续循环
        while let Some(_) = self.parser.peek(0) {
            let current_prec = self.current_precedence();
            if current_prec <= min_precedence {
                break;
            }
            let op = self.parser.consume_token(); // 消耗当前中缀运算符 Token
            left = self.parse_infix(left, op.expect("No such token")); // 解析中缀表达式，将左操作数和运算符传递给中缀解析子句
        }

        left // 返回最终的表达式树
    }

    /// 解析前缀表达式
    fn parse_prefix(&mut self) -> Expr {
        let token = self
            .parser
            .consume_token()
            .expect("Unexpected EOF while parsing prefix expression");

        let token_type = &token.token;
        let parselet = self
            .prefix_parselets
            .get(token_type)
            .unwrap_or_else(|| panic!("No prefix parselet for: {:?}", token_type))
            .clone();

        parselet.parse(self, token)
    }

    /// 解析中缀表达式
    fn parse_infix(&mut self, left: Expr, token: LexedToken) -> Expr {
        // 从中缀解析函数映射表中获取对应 Token 的解析子句，如果不存在则 panic
        let token_type = &token.token;
        let parselet = self.infix_parselets.get(token_type).unwrap().clone();
        parselet.parse(self, left, token) // 调用解析子句的 parse 方法进行解析
    }

    /// 获取当前 Token 的优先级
    fn current_precedence(&mut self) -> PrecedenceLevel {
        // 尝试 peek 下一个 Token，并从 infix_parselets 中查找对应的 InfixParselet
        self.parser
            .peek(0)
            .and_then(|t| self.infix_parselets.get(&t.token))
            .map(|p| p.precedence()) // 如果找到 InfixParselet，则返回其优先级
            .unwrap_or(PrecedenceLevel::Lowest) // 如果没有下一个 Token 或没有对应的 InfixParselet，则返回最低优先级
    }
}

/// PrattParser 构建器结构体，使用构建器模式方便注册解析子句
pub struct PrattParserBuilder {
    prefix_parselets: HashMap<Token, Rc<dyn PrefixParselet>>,
    infix_parselets: HashMap<Token, Rc<dyn InfixParselet>>,
}

impl PrattParserBuilder {
    /// 创建 PrattParserBuilder 实例
    fn new() -> Self {
        Self {
            prefix_parselets: HashMap::new(),
            infix_parselets: HashMap::new(),
        }
    }

    /// 注册前缀解析子句，Builder 模式链式调用
    fn with_prefix(mut self, token: Token, parselet: impl PrefixParselet + 'static) -> Self {
        self.prefix_parselets.insert(token, Rc::new(parselet));
        self
    }

    /// 注册中缀解析子句，Builder 模式链式调用
    fn with_infix(mut self, token: Token, parselet: impl InfixParselet + 'static) -> Self {
        self.infix_parselets.insert(token, Rc::new(parselet));
        self
    }

    /// 构建 PrattParser 实例
    fn build(self, parser: &mut dyn ParserApi) -> PrattParser {
        PrattParser {
            prefix_parselets: self.prefix_parselets,
            infix_parselets: self.infix_parselets,
            parser,
        }
    }
}

pub fn create_pratt_parser(parser: &mut dyn ParserApi) -> PrattParser {
    PrattParserBuilder::new()
        .with_prefix(Token::Integer, NumberParselet)
        .with_prefix(Token::RoundOpen, GroupParselet)
        .with_prefix(Token::Identifier, IdentifierParselet)
        .with_prefix(Token::SquareOpen, ArrayParselet)
        .with_infix(
            Token::Add,
            BinaryOpParselet {
                precedence: PrecedenceLevel::Sum,
                is_right_assoc: false,
            },
        )
        .with_infix(
            Token::Subtract,
            BinaryOpParselet {
                precedence: PrecedenceLevel::Sum,
                is_right_assoc: false,
            },
        )
        .with_infix(
            Token::Multiply,
            BinaryOpParselet {
                precedence: PrecedenceLevel::Product,
                is_right_assoc: false,
            },
        )
        .with_infix(
            Token::Divide,
            BinaryOpParselet {
                precedence: PrecedenceLevel::Product,
                is_right_assoc: false,
            },
        )
        // ( => 中缀表达式启用函数识别
        .with_infix(
            Token::RoundOpen,
            CallParselet {
                precedence: PrecedenceLevel::Call,
            },
        )
        // . 点运算符解析
        .with_infix(Token::Dot, DotParselet)
        // 问号运算符, 用于三元表达式
        .with_infix(Token::QuestionMark, TernaryParselet)
        .with_infix(Token::SquareOpen, IndexParselet)
        .build(parser)
}

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::{
        lexer::{LexedToken, Token, TvLexer},
        span::{Position, Span},
    };
    use std::cell::RefCell;

    /// 模拟 Parser 实现
    struct MockParser {
        tokens: RefCell<Vec<LexedToken>>,
        position: RefCell<usize>,
    }

    impl MockParser {
        fn new(tokens: Vec<LexedToken>) -> Self {
            Self {
                tokens: RefCell::new(tokens),
                position: RefCell::new(0),
            }
        }

        /// 创建带位置信息的 Token
        fn with_positions(tokens: &[(Token, (u32, u32), (u32, u32))]) -> Self {
            let tokens = tokens
                .iter()
                .map(|(t, (sl, sc), (el, ec))| LexedToken {
                    token: t.clone(),
                    span: Span {
                        start: Position {
                            line: *sl,
                            column: *sc,
                        },
                        end: Position {
                            line: *el,
                            column: *ec,
                        },
                    },
                    source_bytes: 0..1, // 简化处理
                })
                .collect();
            Self::new(tokens)
        }
    }

    impl ParserApi for MockParser {
        fn peek(&mut self, n: usize) -> Option<LexedToken> {
            let pos = *self.position.borrow();
            self.tokens.borrow().get(pos + n).cloned()
        }

        fn consume_token(&mut self) -> Option<LexedToken> {
            let mut pos = self.position.borrow_mut();
            let token = self.tokens.borrow().get(*pos).cloned();
            *pos += 1;
            token
        }

        fn token_source(&self, _: &LexedToken) -> &str {
            "" // 测试中暂不需要源字符串
        }
    }

    // 测试工具函数：创建标准 LexedToken
    fn create_token(
        token: Token,
        start_line: u32,
        start_col: u32,
        end_line: u32,
        end_col: u32,
    ) -> LexedToken {
        LexedToken {
            token,
            span: Span {
                start: Position {
                    line: start_line,
                    column: start_col,
                },
                end: Position {
                    line: end_line,
                    column: end_col,
                },
            },
            source_bytes: 0..1,
        }
    }

    // 测试工具函数：创建 LexedToken
    fn int_token() -> LexedToken {
        create_token(Token::Integer, 0, 0, 0, 1)
    }

    fn add_token() -> LexedToken {
        create_token(Token::Add, 0, 2, 0, 3)
    }

    fn mul_token() -> LexedToken {
        create_token(Token::Multiply, 0, 4, 0, 5)
    }

    fn lparen_token() -> LexedToken {
        create_token(Token::RoundOpen, 0, 0, 0, 1)
    }

    fn rparen_token() -> LexedToken {
        create_token(Token::RoundClose, 0, 0, 0, 1)
    }

    #[test]
    fn test_position_tracking() {
        let mut parser = MockParser::with_positions(&[
            (Token::Integer, (0, 0), (0, 1)),
            (Token::Add, (0, 2), (0, 3)),
            (Token::Integer, (0, 4), (0, 5)),
        ]);

        let token1 = parser.consume_token().unwrap();
        assert_eq!(token1.span.start.column, 0);
        assert_eq!(token1.span.end.column, 1);

        let token2 = parser.consume_token().unwrap();
        assert_eq!(token2.span.start.column, 2);
    }

    #[test]
    fn test_single_integer() {
        let mut parser = MockParser::new(vec![int_token()]);
        let mut pratt = create_pratt_parser(&mut parser);

        let expr = pratt.parse_expression(PrecedenceLevel::Lowest);
        assert_eq!(expr, Expr::Integer(1));
    }

    #[test]
    fn test_addition_priority() {
        // 1 + 2 * 3
        let tokens = vec![
            int_token(),
            add_token(),
            int_token(),
            mul_token(),
            int_token(),
        ];
        let mut parser = MockParser::new(tokens);
        let mut pratt = create_pratt_parser(&mut parser);

        let expr = pratt.parse_expression(PrecedenceLevel::Lowest);
        assert_eq!(
            expr,
            Expr::BinaryOp {
                op: Token::Add,
                left: Box::new(Expr::Integer(1)),
                right: Box::new(Expr::BinaryOp {
                    op: Token::Multiply,
                    left: Box::new(Expr::Integer(1)),
                    right: Box::new(Expr::Integer(1)),
                }),
            }
        );
    }

    #[test]
    fn test_right_associative() {
        // 修改乘法为右结合
        let builder = PrattParserBuilder::new()
            .with_prefix(Token::Integer, NumberParselet)
            .with_infix(
                Token::Multiply,
                BinaryOpParselet {
                    precedence: PrecedenceLevel::Product,
                    is_right_assoc: true,
                },
            );

        // 1 * 2 * 3 应该解析为 1 * (2 * 3)
        let tokens = vec![
            int_token(),
            mul_token(),
            int_token(),
            mul_token(),
            int_token(),
        ];
        let mut parser = MockParser::new(tokens);
        let mut pratt = builder.build(&mut parser);

        let expr = pratt.parse_expression(PrecedenceLevel::Lowest);
        assert_eq!(
            expr,
            Expr::BinaryOp {
                op: Token::Multiply,
                left: Box::new(Expr::Integer(1)),
                right: Box::new(Expr::BinaryOp {
                    op: Token::Multiply,
                    left: Box::new(Expr::Integer(1)),
                    right: Box::new(Expr::Integer(1)),
                }),
            }
        );
    }

    #[test]
    fn test_current_precedence() {
        let tokens = vec![mul_token()];
        let mut parser = MockParser::new(tokens);
        let mut pratt = create_pratt_parser(&mut parser);

        // 检查乘法优先级
        assert_eq!(pratt.current_precedence(), PrecedenceLevel::Product);
    }

    #[test]
    #[should_panic(expected = "No prefix parselet for: Add")]
    fn test_invalid_prefix() {
        let mut parser = MockParser::new(vec![add_token()]);
        let mut pratt = create_pratt_parser(&mut parser);
        pratt.parse_expression(PrecedenceLevel::Lowest);
    }

    #[test]
    fn test_mixed_operators() {
        TvLexer::new("1 + 2 * 3 - 4");
        // 1 + 2 * 3 - 4
        let tokens = vec![
            int_token(),
            add_token(),
            int_token(),
            mul_token(),
            int_token(),
            create_token(Token::Subtract, 0, 6, 0, 7), // 添加减法token
            int_token(),
        ];
        let mut parser = MockParser::new(tokens);
        let mut pratt = create_pratt_parser(&mut parser);

        let expr = pratt.parse_expression(PrecedenceLevel::Lowest);
        print!("{:?}", expr);
        assert_eq!(
            expr,
            Expr::BinaryOp {
                op: Token::Subtract,
                left: Box::new(Expr::BinaryOp {
                    op: Token::Add,
                    left: Box::new(Expr::Integer(1)),
                    right: Box::new(Expr::BinaryOp {
                        op: Token::Multiply,
                        left: Box::new(Expr::Integer(1)),
                        right: Box::new(Expr::Integer(1)),
                    }),
                }),
                right: Box::new(Expr::Integer(1)),
            }
        );
    }

    #[test]
    fn test_parentheses() {
        let tokens = vec![
            lparen_token(),
            int_token(),
            add_token(),
            int_token(),
            rparen_token(),
            mul_token(),
            int_token(),
        ];
        let mut parser = MockParser::new(tokens);
        let mut pratt = create_pratt_parser(&mut parser);

        assert_eq!(
            pratt.parse_expression(PrecedenceLevel::Lowest),
            Expr::BinaryOp {
                op: Token::Multiply,
                left: Box::new(Expr::BinaryOp {
                    op: Token::Add,
                    left: Box::new(Expr::Integer(1)),
                    right: Box::new(Expr::Integer(1)),
                }),
                right: Box::new(Expr::Integer(1)),
            }
        );
    }

    #[test]
    fn test_function_call() {
        let tokens = vec![
            create_token(Token::Identifier, 0, 0, 0, 3), // "sum"
            lparen_token(),
            int_token(),
            create_token(Token::Comma, 0, 4, 0, 5),
            int_token(),
            rparen_token(),
        ];
        let mut parser = MockParser::new(tokens);
        let mut pratt = create_pratt_parser(&mut parser);

        assert_eq!(
            pratt.parse_expression(PrecedenceLevel::Lowest),
            Expr::Call {
                func: Box::new(Expr::Identifier("id".to_string())),
                args: vec![Expr::Integer(1), Expr::Integer(1)],
            }
        );
    }

    #[test]
    fn test_member_function_call() {
        let tokens = vec![
            create_token(Token::Identifier, 0, 0, 0, 2),
            create_token(Token::Dot, 0, 3, 0, 4),
            create_token(Token::Identifier, 0, 5, 0, 14),
            lparen_token(),
            create_token(Token::Identifier, 0, 15, 0, 20),
            create_token(Token::Comma, 0, 21, 0, 22),
            int_token(),
            rparen_token(),
        ];

        let mut parser = MockParser::new(tokens);
        let mut pratt = create_pratt_parser(&mut parser);

        assert_eq!(
            pratt.parse_expression(PrecedenceLevel::Lowest),
            Expr::Call {
                func: Box::new(Expr::MemberAccess {
                    object: Box::new(Expr::Identifier("id".to_string())),
                    member: "id".to_string(),
                }),
                args: vec![Expr::Identifier("id".to_string()), Expr::Integer(1),],
            }
        );
    }
}
