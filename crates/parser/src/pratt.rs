use std::collections::HashMap;
use std::rc::Rc;

use lexer::lexer::{LexedToken, Token};
use serde::Deserialize;

use crate::core::ParserCore;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
/// 运算符优先级枚举
pub enum PrecedenceLevel {
    /// 最低优先级
    Lowest = 0,
    /// 赋值运算符
    Assignment = 1,
    /// 条件运算符（三元表达式）
    Conditional = 2,
    Comparison = 5,
    /// 加法和减法运算符
    Sum = 10,
    /// 乘法和除法运算符
    Product = 20,
    /// 指数运算符
    Exponent = 30,
    /// 前缀运算符，如一元加减
    Prefix = 40,
    /// 函数调用运算符
    Call = 50,
}

impl PrecedenceLevel {
    /// 获取优先级的值（数值越大，优先级越高）
    fn value(self) -> u32 {
        self as u32
    }

    fn predecessor(self) -> Self {
        match self {
            Self::Exponent => Self::Product,
            Self::Product => Self::Sum,
            Self::Sum => Self::Comparison,
            Self::Comparison => Self::Conditional,
            Self::Conditional => Self::Assignment,
            _ => Self::Lowest,
        }
    }
}

#[derive(Debug, PartialEq, Deserialize)]
pub enum BinaryOp {
    Add,             // +
    AddAssign,       // +=
    Subtract,        // -
    SubtractAssign,  // -=
    Multiply,        // *
    MultiplyAssign,  // *=
    Divide,          // /
    DivideAssign,    // /=
    Remainder,       // %
    RemainderAssign, // %=
    Assign,          // =
    Equal,           // ==
    NotEqual,        // !=
    Greater,         // >
    GreaterOrEqual,  // >=
    Less,            // <
    LessOrEqual,     // <=
    And              // &&
}

impl BinaryOp {
    /// 从原始字符转换为 `BinaryOp` 枚举值
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "+" => Some(BinaryOp::Add),
            "+=" => Some(BinaryOp::AddAssign),
            "-" => Some(BinaryOp::Subtract),
            "-=" => Some(BinaryOp::SubtractAssign),
            "*" => Some(BinaryOp::Multiply),
            "*=" => Some(BinaryOp::MultiplyAssign),
            "/" => Some(BinaryOp::Divide),
            "/=" => Some(BinaryOp::DivideAssign),
            "%" => Some(BinaryOp::Remainder),
            "%=" => Some(BinaryOp::RemainderAssign),
            "=" => Some(BinaryOp::Assign),
            "==" => Some(BinaryOp::Equal),
            "!=" => Some(BinaryOp::NotEqual),
            ">" => Some(BinaryOp::Greater),
            ">=" => Some(BinaryOp::GreaterOrEqual),
            "<" => Some(BinaryOp::Less),
            "<=" => Some(BinaryOp::LessOrEqual),
            "&&" => Some(BinaryOp::And),
            _ => None, // 未知字符返回 None
        }
    }
}

/// 表达式枚举，表示解析后的表达式树
#[derive(Debug, PartialEq, Deserialize)]
pub enum Expr {
    /// 整数字面量
    Integer(i64),
    Float(f64),
    /// 二元运算表达式
    BinaryOp {
        /// 运算符
        op: BinaryOp,
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
    fn parse(&self, parser: &mut PrattParser, token: LexedToken) -> Expr {
        let str_value = parser.parser.token_source(&token);
        // 目前只处理 Integer Token，实际应用中需要根据 Token 类型和值进行更精细的处理
        match token.token {
            Token::Integer => {
                // 解析为整数
                let int_value: i64 = str_value.parse().expect("Failed to parse integer");
                Expr::Integer(int_value)
            }
            Token::Float => {
                // 解析为浮点数
                let float_value: f64 = str_value.parse().expect("Failed to parse float");
                Expr::Float(float_value)
            }
            _ => {
                panic!("Unexpected token")
            }
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
        parser.parser.consume_token(); // 消耗一个 ]
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
        // 将token的原始值转成BinaryOp
        let b: BinaryOp = BinaryOp::from_str(parser.parser.token_source(&token)).expect("unknown Binary Operator");
        
        Expr::BinaryOp {
            op: b,
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
    fn parse(&self, parser: &mut PrattParser, token: LexedToken) -> Expr {
        let id = parser.parser.token_source(&token);

        if let Token::Identifier = &token.token {
            Expr::Identifier(id.into())
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

        let member = parser.parser.token_source(&member_token).to_string();
        
        // 类型检查
        if !matches!(member_token.token, Token::Identifier) {
            panic!("Expected identifier after '.'");
        }

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
        // 解析then分支时使用Conditional优先级
        let then_expr = parser.parse_expression(self.precedence().predecessor());

        // 验证并消费冒号
        let colon_token = parser.parser.consume_token().expect("Expected ':' in ternary expression");
        if colon_token.token != Token::Colon {
            panic!("Expected ':' after then expression, found {:?}", colon_token.token);
        }

        // 调整else分支解析优先级为Conditional的前序优先级
        let else_prec = PrecedenceLevel::Conditional.predecessor();
        let else_expr = parser.parse_expression(else_prec);

        Expr::Ternary {
            cond_expr: Box::new(cond),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
        }
    }

    fn precedence(&self) -> PrecedenceLevel {
        PrecedenceLevel::Conditional
    }
}

// 下标访问解析器
struct IndexParselet;
impl InfixParselet for IndexParselet {
    fn parse(&self, parser: &mut PrattParser, left: Expr, _: LexedToken) -> Expr {
        let index = parser.parse_expression(PrecedenceLevel::Lowest);
        // 消耗 ]
        parser.parser.consume_token();
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
pub struct PrattParser<'core> {
    /// 前缀解析函数映射表，key 是 Token 类型，value 是实现了 PrefixParselet trait 的解析器
    prefix_parselets: HashMap<Token, Rc<dyn PrefixParselet>>,
    /// 中缀解析函数映射表，key 是 Token 类型，value 是实现了 InfixParselet trait 的解析器
    infix_parselets: HashMap<Token, Rc<dyn InfixParselet>>,
    parser: &'core mut ParserCore<'core>,
}

impl<'a> PrattParser<'a> {
    /// 创建 PrattParser 实例
    pub fn new(parser: &'a mut ParserCore<'a>) -> Self {
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
    pub fn parse_expression(&mut self, min_precedence: PrecedenceLevel) -> Expr {
        // 先解析前缀表达式
        let mut left = self.parse_prefix();

        // 消费下一个无效token
        self.parser.consume_until_token();
        // 确保只有在存在下一个token且优先级足够高时才继续循环
        while let Some(next_token) = self.parser.peek(0) {
            if next_token.token == Token::EOF {
                break;
            }

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
        // 消费无效token
        self.parser.consume_until_token();
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
    pub fn build<'a>(self, parser: &'a mut ParserCore<'a>) -> PrattParser<'a> {
        PrattParser {
            prefix_parselets: self.prefix_parselets,
            infix_parselets: self.infix_parselets,
            parser,
        }
    }
}

pub fn create_pratt_parser<'a>(parser: &'a mut ParserCore<'a>) -> PrattParser<'a> {
    PrattParserBuilder::new()
        .with_prefix(Token::Integer, NumberParselet)
        .with_prefix(Token::Float, NumberParselet)
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
        .with_infix(
            Token::Assign,
            BinaryOpParselet {
                precedence: PrecedenceLevel::Assignment,
                is_right_assoc: true, // 赋值运算符右结合
            },
        )
        // . 点运算符解析
        .with_infix(Token::Dot, DotParselet)
        // 问号运算符, 用于三元表达式
        .with_infix(Token::QuestionMark, TernaryParselet)
        .with_infix(
            Token::Greater,
            BinaryOpParselet {
                precedence: PrecedenceLevel::Comparison,
                is_right_assoc: false,
            },
        )
        .with_infix(
            Token::Less,
            BinaryOpParselet {
                precedence: PrecedenceLevel::Comparison,
                is_right_assoc: false,
            },
        )
        .with_infix(
            Token::GreaterOrEqual,
            BinaryOpParselet {
                precedence: PrecedenceLevel::Comparison,
                is_right_assoc: false,
            },
        )
        .with_infix(
            Token::LessOrEqual,
            BinaryOpParselet {
                precedence: PrecedenceLevel::Comparison,
                is_right_assoc: false,
            },
        )
        // 数组访问
        .with_infix(Token::SquareOpen, IndexParselet)
        .build(parser)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::ParserCoreBuilder;
    use lexer::lexer::Token;
    use test_utils::print_tokens;

    pub fn parse_expr(input: &str) -> Expr {
        let mut core = ParserCoreBuilder::build(input);
        let mut pratt = create_pratt_parser(&mut core);
        pratt.parse_expression(PrecedenceLevel::Lowest)
    }

    #[test]
    fn test_operator_precedence() {
        let source = "1   + 2 *  3";
        print_tokens(source);
        let expr = parse_expr(source);
        print!("{:?}", expr);

        assert_eq!(
            expr,
            Expr::BinaryOp {
                op: BinaryOp::Add,
                left: Box::new(Expr::Integer(1)),
                right: Box::new(Expr::BinaryOp {
                    op: BinaryOp::Multiply,
                    left: Box::new(Expr::Integer(2)),
                    right: Box::new(Expr::Integer(3)),
                })
            }
        );
    }

    #[test]
    fn test_single_integer() {
        let source = "10";
        print_tokens(source);
        let expr = parse_expr(source);
        print!("{:?}", expr);

        assert_eq!(expr, Expr::Integer(10));
    }

    #[test]
    fn test_right_associative() {
        let source = "1 * 2 * 3";
        print_tokens(source);
        let expr = parse_expr(source);
        print!("{:?}", expr);

        assert_eq!(
            expr,
            Expr::BinaryOp {
                op: BinaryOp::Multiply,
                left: Box::new(Expr::BinaryOp {
                    op: BinaryOp::Multiply,
                    left: Box::new(Expr::Integer(1)),
                    right: Box::new(Expr::Integer(2)),
                }),
                right: Box::new(Expr::Integer(3)),
            }
        );
    }

    #[test]
    #[should_panic(expected = "No prefix parselet for: Add")]
    fn test_invalid_prefix() {
        let source = "+";
        parse_expr(source);
    }

    #[test]
    fn test_mixed_operators() {
        let source = "1 + 2 * 3 - 4";
        let expr = parse_expr(source);
        print!("{:?}", expr);
        assert_eq!(
            expr,
            Expr::BinaryOp {
                op: BinaryOp::Subtract,
                left: Box::new(Expr::BinaryOp {
                    op: BinaryOp::Add,
                    left: Box::new(Expr::Integer(1)),
                    right: Box::new(Expr::BinaryOp {
                        op: BinaryOp::Multiply,
                        left: Box::new(Expr::Integer(2)),
                        right: Box::new(Expr::Integer(3)),
                    }),
                }),
                right: Box::new(Expr::Integer(4)),
            }
        );
    }

    #[test]
    fn test_parentheses() {
        let source = "(1 + 1) * 1";
        let expr = parse_expr(source);
        assert_eq!(
            expr,
            Expr::BinaryOp {
                op: BinaryOp::Multiply,
                left: Box::new(Expr::BinaryOp {
                    op: BinaryOp::Add,
                    left: Box::new(Expr::Integer(1)),
                    right: Box::new(Expr::Integer(1)),
                }),
                right: Box::new(Expr::Integer(1)),
            }
        );
    }

    #[test]
    fn test_function_call() {
        let source = "sum(1, 1)";
        let expr = parse_expr(source);
        assert_eq!(
            expr,
            Expr::Call {
                func: Box::new(Expr::Identifier("sum".to_string())),
                args: vec![Expr::Integer(1), Expr::Integer(1)],
            }
        );
    }

    #[test]
    fn test_member_function_call() {
        let source = "ta.sma(id, 1)";
        let expr = parse_expr(source);
        println!("{:?}", expr);
        assert_eq!(
            expr,
            Expr::Call {
                func: Box::new(Expr::MemberAccess {
                    object: Box::new(Expr::Identifier("ta".to_string())),
                    member: "sma".to_string(),
                }),
                args: vec![Expr::Identifier("id".to_string()), Expr::Integer(1),],
            }
        );
    }

    #[test]
    fn test_member_function_chain_call() {
        let source = "a.b.c.d";
        let expr = parse_expr(source);
        println!("{:?}", expr);

    }

    #[test]
    fn test_simple_ternary() {
        let source = "a ? 1 : 2";
        let expr = parse_expr(source);
        assert_eq!(
            expr,
            Expr::Ternary {
                cond_expr: Box::new(Expr::Identifier("a".to_string())),
                then_expr: Box::new(Expr::Integer(1)),
                else_expr: Box::new(Expr::Integer(2)),
            }
        );
    }

    #[test]
    fn test_nested_ternary() {
        let source = "a ? b : c ? d : e";
        let expr = parse_expr(source);
        assert_eq!(
            expr,
            Expr::Ternary {
                cond_expr: Box::new(Expr::Identifier("a".to_string())),
                then_expr: Box::new(Expr::Identifier("b".to_string())),
                else_expr: Box::new(Expr::Ternary {
                    cond_expr: Box::new(Expr::Identifier("c".to_string())),
                    then_expr: Box::new(Expr::Identifier("d".to_string())),
                    else_expr: Box::new(Expr::Identifier("e".to_string())),
                }),
            }
        );
    }

    #[test]
    #[should_panic(expected = "Expected ':'")]
    fn test_invalid_ternary_missing_colon() {
        let source = "a ? 1 2";
        parse_expr(source);
    }

    #[test]
    fn test_ternary_with_operations() {
        let source = "a > 0 ? x + 1 : y * 2";
        let expr = parse_expr(source);
        assert_eq!(
            expr,
            Expr::Ternary {
                cond_expr: Box::new(Expr::BinaryOp {
                    op: BinaryOp::Greater,
                    left: Box::new(Expr::Identifier("a".to_string())),
                    right: Box::new(Expr::Integer(0)),
                }),
                then_expr: Box::new(Expr::BinaryOp {
                    op: BinaryOp::Add,
                    left: Box::new(Expr::Identifier("x".to_string())),
                    right: Box::new(Expr::Integer(1)),
                }),
                else_expr: Box::new(Expr::BinaryOp {
                    op: BinaryOp::Multiply,
                    left: Box::new(Expr::Identifier("y".to_string())),
                    right: Box::new(Expr::Integer(2)),
                }),
            }
        );
    }

    #[test]
    fn test_nested_ternary_right_assoc() {
        let source = "a ? b : c ? d : e";
        let expr = parse_expr(source);
        assert_eq!(
            expr,
            Expr::Ternary {
                cond_expr: Box::new(Expr::Identifier("a".into())),
                then_expr: Box::new(Expr::Identifier("b".into())),
                else_expr: Box::new(Expr::Ternary {
                    cond_expr: Box::new(Expr::Identifier("c".into())),
                    then_expr: Box::new(Expr::Identifier("d".into())),
                    else_expr: Box::new(Expr::Identifier("e".into())),
                })
            }
        );
    }

    // 混合逻辑运算符
    // #[test]
    // fn test_ternary_with_logical_ops() {
    //     let source = "x > 5 && y < 10 ? a || b : c && d";
    //     let expr = parse_expr(source);
    //     assert_eq!(
    //         expr,
    //         Expr::Ternary {
    //             cond_expr: Box::new(Expr::BinaryOp {
    //                 op: Token::And,
    //                 left: Box::new(Expr::BinaryOp {
    //                     op: Token::Greater,
    //                     left: Box::new(Expr::Identifier("x".into())),
    //                     right: Box::new(Expr::Integer(5)),
    //                 }),
    //                 right: Box::new(Expr::BinaryOp {
    //                     op: Token::Less,
    //                     left: Box::new(Expr::Identifier("y".into())),
    //                     right: Box::new(Expr::Integer(10)),
    //                 }),
    //             }),
    //             then_expr: Box::new(Expr::BinaryOp {
    //                 op: Token::Or,
    //                 left: Box::new(Expr::Identifier("a".into())),
    //                 right: Box::new(Expr::Identifier("b".into())),
    //             }),
    //             else_expr: Box::new(Expr::BinaryOp {
    //                 op: Token::And,
    //                 left: Box::new(Expr::Identifier("c".into())),
    //                 right: Box::new(Expr::Identifier("d".into())),
    //             }),
    //         }
    //     );
    // }

    // 与赋值结合 ✅
    #[test]
    fn test_ternary_with_assignment() {
        let source = "result = condition ? value1 : value2";
        let expr = parse_expr(source);
        assert_eq!(
            expr,
            Expr::BinaryOp {
                op: BinaryOp::Assign,
                left: Box::new(Expr::Identifier("result".into())),
                right: Box::new(Expr::Ternary {
                    cond_expr: Box::new(Expr::Identifier("condition".into())),
                    then_expr: Box::new(Expr::Identifier("value1".into())),
                    else_expr: Box::new(Expr::Identifier("value2".into())),
                }),
            }
        );
    }

    #[test]
    fn test_assignment() {
        let source = "result = 1";
        let expr = parse_expr(source);
        println!("{:?}", expr);
    }

    // 函数调用参数 ✅
    #[test]
    fn test_ternary_in_function_args() {
        let source = "calculate(a ? b : c, d ? e : f)";
        let expr = parse_expr(source);
        assert_eq!(
            expr,
            Expr::Call {
                func: Box::new(Expr::Identifier("calculate".into())),
                args: vec![
                    Expr::Ternary {
                        cond_expr: Box::new(Expr::Identifier("a".into())),
                        then_expr: Box::new(Expr::Identifier("b".into())),
                        else_expr: Box::new(Expr::Identifier("c".into())),
                    },
                    Expr::Ternary {
                        cond_expr: Box::new(Expr::Identifier("d".into())),
                        then_expr: Box::new(Expr::Identifier("e".into())),
                        else_expr: Box::new(Expr::Identifier("f".into())),
                    }
                ],
            }
        );
    }

    // 嵌套对象访问 ❌
    #[test]
    fn test_ternary_with_member_access() {
        let source = "obj.prop ? data.list[0] : config.default";
        let expr = parse_expr(source);
        assert_eq!(
            expr,
            Expr::Ternary {
                cond_expr: Box::new(Expr::MemberAccess {
                    object: Box::new(Expr::Identifier("obj".into())),
                    member: "prop".into(),
                }),
                then_expr: Box::new(Expr::IndexAccess {
                    array: Box::new(Expr::MemberAccess {
                        object: Box::new(Expr::Identifier("data".into())),
                        member: "list".into(),
                    }),
                    index: Box::new(Expr::Integer(0)),
                }),
                else_expr: Box::new(Expr::MemberAccess {
                    object: Box::new(Expr::Identifier("config".into())),
                    member: "default".into(),
                }),
            }
        );
    }

    // 多级嵌套 👌
    #[test]
    fn test_multi_level_nesting() {
        let source = "a ? b ? c : d : e ? f : g";
        let expr = parse_expr(source);
        assert_eq!(
            expr,
            Expr::Ternary {
                cond_expr: Box::new(Expr::Identifier("a".into())),
                then_expr: Box::new(Expr::Ternary {
                    cond_expr: Box::new(Expr::Identifier("b".into())),
                    then_expr: Box::new(Expr::Identifier("c".into())),
                    else_expr: Box::new(Expr::Identifier("d".into())),
                }),
                else_expr: Box::new(Expr::Ternary {
                    cond_expr: Box::new(Expr::Identifier("e".into())),
                    then_expr: Box::new(Expr::Identifier("f".into())),
                    else_expr: Box::new(Expr::Identifier("g".into())),
                }),
            }
        );
    }

    // 与指数运算符结合
    // #[test]
    // fn test_ternary_with_exponent() {
    //     let source = "x ^ 2 > 100 ? y ^ 3 : z ^ 4";
    //     let expr = parse_expr(source);
    //     assert_eq!(
    //         expr,
    //         Expr::Ternary {
    //             cond_expr: Box::new(Expr::BinaryOp {
    //                 op: Token::Greater,
    //                 left: Box::new(Expr::BinaryOp {
    //                     op: Token::Exponent,
    //                     left: Box::new(Expr::Identifier("x".into())),
    //                     right: Box::new(Expr::Integer(2)),
    //                 }),
    //                 right: Box::new(Expr::Integer(100)),
    //             }),
    //             then_expr: Box::new(Expr::BinaryOp {
    //                 op: Token::Exponent,
    //                 left: Box::new(Expr::Identifier("y".into())),
    //                 right: Box::new(Expr::Integer(3)),
    //             }),
    //             else_expr: Box::new(Expr::BinaryOp {
    //                 op: Token::Exponent,
    //                 left: Box::new(Expr::Identifier("z".into())),
    //                 right: Box::new(Expr::Integer(4)),
    //             }),
    //         }
    //     );
    // }

    // 数组操作结合 ❌
    #[test]
    fn test_ternary_with_array_ops() {
        let source = "list.length > 0 ? list[0] : [1, 2, 3]";
        let expr = parse_expr(source);
        assert_eq!(
            expr,
            Expr::Ternary {
                cond_expr: Box::new(Expr::BinaryOp {
                    op: BinaryOp::Greater,
                    left: Box::new(Expr::MemberAccess {
                        object: Box::new(Expr::Identifier("list".into())),
                        member: "length".into(),
                    }),
                    right: Box::new(Expr::Integer(0)),
                }),
                then_expr: Box::new(Expr::IndexAccess {
                    array: Box::new(Expr::Identifier("list".into())),
                    index: Box::new(Expr::Integer(0)),
                }),
                else_expr: Box::new(Expr::ArrayLiteral(vec![
                    Expr::Integer(1),
                    Expr::Integer(2),
                    Expr::Integer(3),
                ])),
            }
        );
    }

}

