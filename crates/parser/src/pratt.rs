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
}

/// Pratt Parser 结构体
pub struct PrattParser<'a> {
    /// 前缀解析函数映射表，key 是 Token 类型，value 是实现了 PrefixParselet trait 的解析器
    prefix_parselets: HashMap<Token, Rc<dyn PrefixParselet>>,
    /// 中缀解析函数映射表，key 是 Token 类型，value 是实现了 InfixParselet trait 的解析器
    infix_parselets: HashMap<Token, Rc<dyn InfixParselet>>,
    parser: &'a mut dyn ParserApi,
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

        // 循环处理中缀运算符，直到遇到优先级低于 min_precedence 的运算符
        while min_precedence < self.current_precedence() {
            let op = self.parser.consume_token(); // 消耗当前中缀运算符 Token
            left = self.parse_infix(left, op.expect("No such token")); // 解析中缀表达式，将左操作数和运算符传递给中缀解析子句
        }

        left // 返回最终的表达式树
    }

    /// 解析前缀表达式
    fn parse_prefix(&mut self) -> Expr {
        let token = self.parser.consume_token()
            .expect("Unexpected EOF while parsing prefix expression");
    
        let token_type = &token.token;
        let parselet = self.prefix_parselets
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
        // 注册数字前缀解析
        .with_prefix(Token::Integer, NumberParselet)
        // 注册加法中缀解析（左结合，优先级10）
        .with_infix(
            Token::Add,
            BinaryOpParselet {
                precedence: PrecedenceLevel::Sum,
                is_right_assoc: false,
            },
        )
        // 注册乘法中缀解析（左结合，优先级20）
        .with_infix(
            Token::Multiply,
            BinaryOpParselet {
                precedence: PrecedenceLevel::Product,
                is_right_assoc: false,
            },
        )
        .build(parser)
}
