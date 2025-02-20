use std::{collections::VecDeque, ops::Range};

use crate::{
    fsm::State, helper::validate_float, span::{Position, Span}
};

/// the tokens thats can emerge from the lexer
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum Token {
    Error,
    Whitespace,
    NewLine,
    Comment,
    Assign,

    Keyword,    // 关键字: var, float, if等
    Identifier, // 标识符
    Operator,   // 运算符

    StringStart,   // 字符串开始
    StringEnd,     // 字符串结束
    StringLiteral, // 字符串内容

    Integer, // 整数
    Float,   // 浮点数

    // 符号
    LParen,   // 左括号: (
    RParen,   // 右括号: )
    LBracket, // 左方括号: [
    RBracket, // 右方括号: ]
    Comma,    // 逗号: ,
    Dot,      // 点号: .
    EOF,      // 文件结束符
}

#[derive(Clone)]
pub struct TokenLexer<'a> {
    // 状态
    pub state: State,
    // 原始输入
    source: &'a str,
    // 当前字节位置
    current_byte: usize,
    // 当前Token的起始字节位置
    token_start_byte: usize,
    previous_byte: usize,
    // 前一个token
    previous_token: Option<Token>,
    // 存储中间值
    pub partial_number: Option<String>,
    // 当前 Token 的字节范围
    span: Span,
}

impl<'a> TokenLexer<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            state: State::Initial,
            source,
            previous_byte: 0,
            current_byte: 0,
            token_start_byte: 0,
            previous_token: None,
            partial_number: None,
            span: Span::default(),
        }
    }

    /// 开始记录新Token的位置
    fn start_token(&mut self) {
        self.token_start_byte = self.current_byte;
    }

    /// 获取当前Token的字节范围
    fn current_token_bytes(&self) -> Range<usize> {
        self.token_start_byte..self.current_byte
    }

    // Advance along the current line by a number of bytes
    //
    // The characters being advanced over should all be ANSI,
    // i.e. the byte count must match the character count.
    //
    // If the characters have been read as UTF-8 then advance_line_utf8 should be used instead.
    pub fn advance_line(&mut self, char_bytes: usize) {
        self.advance_line_utf8(char_bytes, char_bytes);
    }

    // Advance along the current line by a number of bytes, with a UTF-8 character count
    pub fn advance_line_utf8(&mut self, char_bytes: usize, char_count: usize) {
        // Defer to advance_to_position to update the position
        let new_position = Position {
            line: self.span.end.line,
            column: self.span.end.column + char_count as u32,
        };
        self.advance_to_position(char_bytes, new_position);
    }

    pub fn advance_to_position(&mut self, char_bytes: usize, position: Position) {
        self.previous_byte = self.current_byte;
        self.current_byte += char_bytes;

        self.span = Span {
            start: self.span.end,
            end: position,
        };
    }

    /// 消耗空白字符（不含换行符）
    ///
    /// ## 处理范围
    /// - 空格 (` `)
    /// - 制表符 (`\t`)
    /// - 其他 Unicode 空白符（遵循 Pine Script 的 ASCII 限制）
    ///
    /// ## Pine Script 关键用例
    /// ```pine
    /// // 用例1：缩进风格（无语法意义）
    /// if condition
    ///     strategy.entry("Long")  // 缩进被完全忽略
    ///
    /// // 用例2：对齐操作符
    /// avg = sma(close, 20)
    ///          + sma(open, 20)  // 前导空格被清理
    /// ```
    ///
    /// ## 行为特征
    /// - 不生成任何 Token
    /// - 直接修改词法器位置
    /// - 兼容 Pine Script 的 ASCII 空格规范
    ///
    /// ## 实现细节
    /// 使用线性扫描而非正则表达式，优化性能
    pub fn consume_whitespace(&mut self) {
        while let Some(c) = self.peek_char() {
            if c.is_whitespace() && c != '\n' && c != '\r' {
                // Pine Script仅允许ASCII空格
                self.advance_line(1); // 空格总是ANSI字符
            } else {
                break;
            }
        }
    }

    /// 处理换行符序列并返回 `Token::NewLine`
    ///
    /// ## 功能
    /// - 识别所有平台换行符：`\n` (Unix)、`\r\n` (Windows)、`\r` (旧版 Mac)
    /// - 更新词法器位置到新行首
    /// - 自动清理后续缩进（通过调用 `consume_whitespace`）
    ///
    /// ## Pine Script 关键用例
    /// ```pine
    /// // 用例1：多语句分隔
    /// a = 1  // <-- 换行触发语句结束
    /// b = 2  // <-- 新语句开始
    ///
    /// // 用例2：Windows 文件兼容
    /// plot(close)\r\nplot(open)
    ///
    /// // 用例3：表达式中的换行（需括号包裹）
    /// c = (close +
    ///      open) / 2  // 此处换行不触发语句结束
    /// ```
    ///
    /// ## 算法
    /// 1. 聚合处理连续换行符（如 `\n\n\n`）
    /// 2. 对每个换行符组合精确计算行号增量
    /// 3. 重置列计数器到新行首
    ///
    /// ## 副作用
    /// - 修改 `span.end` 的行号和列号
    /// - 可能改变 `current_byte` 的位置
    /// - 自动触发空白符清理
    pub fn consume_newline(&mut self) -> Token {
        let mut any_newline = false;

        // 阶段1：处理所有连续换行符
        while let Some(c) = self.peek_char() {
            match c {
                '\n' => {
                    any_newline = true;
                    self.advance_line(1); // 更新 span.end.line
                }
                '\r' => {
                    any_newline = true;
                    self.advance_line(1); // 消耗 \r
                    if self.peek_char() == Some('\n') {
                        self.advance_line(1); // 消耗 \n
                    }
                }
                _ => break,
            }
        }

        // 阶段2：处理换行后状态
        if any_newline {
            // 重置列号并消耗缩进
            self.advance_to_position(
                0,
                Position {
                    line: self.span.end.line,
                    column: 0,
                },
            );
            self.consume_whitespace();
        }
        Token::NewLine
    }

    fn consume_symbol(&mut self, remaining: &str) -> Option<Token> {
        use Token::*;

        macro_rules! check_symbol {
            ($token_str:expr, $token:ident) => {
                if remaining.starts_with($token_str) {
                    self.advance_line($token_str.len());
                    return Some($token);
                }
            };
        }

        check_symbol!("=", Assign);

        None
    }

    pub fn get_next_token(&mut self) -> Option<Token> {
        let processor: Box<dyn StateProcessor> = match self.state {
            State::Initial => Box::new(InitialProcessor),
            State::InInteger => Box::new(IntegerProcessor),
            State::InFloat => Box::new(FloatProcessor {
                phase: FloatPhase::Mantissa,
            }),
            State::InIdentifier => Box::new(IdentifierProcessor),
            State::InSlash => Box::new(SlashProcessor),
            State::InOperator => Box::new(OperatorProcessor),
            State::InComment => Box::new(CommentProcessor),
            State::StringLiteral => Box::new(StringProcessor),
            _ => Box::new(ErrorProcessor),
        };

        processor.process(self)
    }
}

// 状态转换
impl<'a> TokenLexer<'a> {
    /// 重置到初始状态
    pub fn reset_state(&mut self) {
        self.state = State::Initial;
    }

    /// 统一状态转换方法（处理简单转换）
    pub fn transition(&mut self, new_state: State) -> Option<Token> {
        self.state = new_state;
        self.get_next_token()
    }

    /// 处理整数到浮点数的状态转换
    pub fn transition_to_float(&mut self, int_part: String) -> Option<Token> {
        // 存储整数部分并添加小数点
        let mut buffer = int_part;
        buffer.push('.'); // 添加当前字符
        self.advance_line(1); // 消耗小数点

        // 保存中间结果并转换状态
        self.partial_number = Some(buffer);
        self.state = State::InFloat;

        // 继续处理浮点数部分
        self.get_next_token()
    }
}

// ================ 状态处理器 trait ================
trait StateProcessor {
    fn process(&self, lexer: &mut TokenLexer) -> Option<Token>;
}

// ================ 具体处理器实现 ================
/// 初始状态处理器
struct InitialProcessor;
impl StateProcessor for InitialProcessor {
    fn process(&self, lexer: &mut TokenLexer) -> Option<Token> {
        let c = lexer.peek_char()?;
        match c {
            // 数字处理
            '0'..='9' => lexer.transition(State::InInteger),
            // 标识符处理
            'a'..='z' | 'A'..='Z' | '_' => lexer.transition(State::InIdentifier),
            // 斜杠 -> 进入斜杠处理
            '/' => lexer.transition(State::InSlash),
            // 运算符 -> 进入运算符状态
            '=' | '>' | '<' | '!' | '+' | '-' | '*' | '%' | ':' => {
                lexer.transition(State::InOperator)
            }
            // 双引号, 字符串字面量处理
            '"' => {
                // 消耗一个双引号
                lexer.advance_line(1);
                lexer.state = State::StringLiteral; // 转换到字符串状态
                Some(Token::StringStart) // 明确返回开始标记
            }
            // 空格
            ' ' | '\t' => {
                lexer.consume_whitespace();
                // 不生成空格
                Some(Token::Whitespace)
            }
            // 换行处理
            '\n' | '\r' => Some(lexer.consume_newline()),
            _ => {
                lexer.advance_line(1);
                Some(Token::Error)
            }
        }
    }
}

/// 整数状态处理器
struct IntegerProcessor;
impl StateProcessor for IntegerProcessor {
    fn process(&self, lexer: &mut TokenLexer) -> Option<Token> {
        let mut buffer = String::new();
        while let Some(c) = lexer.peek_char() {
            match c {
                '0'..='9' => {
                    buffer.push(c);
                    lexer.advance_line(1)
                }
                '.' => return lexer.transition_to_float(buffer),
                _ => break,
            }
        }
        lexer.reset_state();
        Some(Token::Integer)
    }
}

/// 浮点数处理器
#[derive(Clone, PartialEq)]
enum FloatPhase {
    Mantissa,       // 处理尾数部分（必须包含小数点）
    ExponentMarker, // 遇到e/E
    ExponentSign,   // 处理指数符号
    ExponentDigit,  // 处理指数数字
}

struct FloatProcessor {
    phase: FloatPhase,
}

impl StateProcessor for FloatProcessor {
    fn process(&self, lexer: &mut TokenLexer) -> Option<Token> {
        let mut buffer = lexer.partial_number.take().unwrap_or_default();
        let mut phase = self.phase.clone();
        let mut has_dot = buffer.contains('.');

        while let Some(c) = lexer.peek_char() {
            match (&phase, c) {
                // 尾数部分处理
                (FloatPhase::Mantissa, '0'..='9') => buffer.push(c),
                (FloatPhase::Mantissa, '.') if !has_dot => {
                    has_dot = true;
                    buffer.push(c);
                }

                // 指数标识符处理
                (FloatPhase::Mantissa, 'e' | 'E') if has_dot => {
                    buffer.push(c);
                    phase = FloatPhase::ExponentMarker;
                }

                // 指数符号处理
                (FloatPhase::ExponentMarker, '+' | '-') => {
                    buffer.push(c);
                    phase = FloatPhase::ExponentSign;
                }

                // 指数数字处理
                (FloatPhase::ExponentSign, '0'..='9') | (FloatPhase::ExponentDigit, '0'..='9') => {
                    buffer.push(c);
                    phase = FloatPhase::ExponentDigit;
                }

                // 非法字符中断
                _ => break,
            }
            lexer.advance_line(1);
        }

        // 合法性验证
        lexer.reset_state();
        let valid = validate_float(&buffer);
        Some(if valid { Token::Float } else { Token::Error })
    }
}

/// 标识符处理器
struct IdentifierProcessor;
impl StateProcessor for IdentifierProcessor {
    fn process(&self, lexer: &mut TokenLexer) -> Option<Token> {
        let mut ident = String::new();
        while let Some(c) = lexer.peek_char() {
            if c.is_alphanumeric() || c == '_' {
                ident.push(c);
                lexer.advance_line(1)
            } else {
                break;
            }
        }
        lexer.reset_state();
        Some(lexer.keyword_or_identifier(&ident))
    }
}

// ================== 斜杠处理器 ==================
struct SlashProcessor;
impl StateProcessor for SlashProcessor {
    fn process(&self, lexer: &mut TokenLexer) -> Option<Token> {
        let next_char = lexer.peek_char()?;

        match next_char {
            // 遇到第二个斜杠 -> 单行注释
            '/' => {
                lexer.advance_line(1);
                lexer.state = State::InComment;
                Some(Token::Comment) // 初始空注释
            }

            // 其他情况 -> 普通除法运算符
            _ => {
                lexer.reset_state();
                Some(Token::Operator)
            }
        }
    }
}

/// ================== 运算符处理器 ==================
struct OperatorProcessor;
impl StateProcessor for OperatorProcessor {
    fn process(&self, lexer: &mut TokenLexer) -> Option<Token> {
        let first_char = lexer.peek_char()?;
        let mut operator = String::from(first_char);
        lexer.advance_line(1);

        // 检查可能的多字符运算符
        if let Some(next_c) = lexer.source[lexer.current_byte..].chars().next() {
            let combined = match (first_char, next_c) {
                ('=', '=') => "==",
                ('!', '=') => "!=",
                ('>', '=') => ">=",
                ('<', '=') => "<=",
                ('+', '+') => "++",
                ('-', '-') => "--",
                _ => "",
            };

            if !combined.is_empty() {
                operator.push(next_c);
                lexer.advance_line(1);
            }
        }

        lexer.reset_state();
        Some(Token::Operator)
    }
}

// ================== 单行注释处理器 ==================
struct CommentProcessor;
impl StateProcessor for CommentProcessor {
    fn process(&self, lexer: &mut TokenLexer) -> Option<Token> {
        let _comment_content = String::new();

        // 消耗所有字符直到行尾
        while let Some(c) = lexer.peek_char() {
            if c == '\n' || c == '\r' {
                break;
            }
            lexer.advance_line(1)
        }
        Some(Token::Comment)
    }
}

// ================== 字符串处理器 ==================
struct StringProcessor;
impl StateProcessor for StringProcessor {
    fn process(&self, lexer: &mut TokenLexer) -> Option<Token> {
        let mut value = String::new();
        let mut escape = false;

        loop {
            match lexer.peek_char() {
                Some('"') if !escape => {
                    lexer.advance_line(1); // 消耗结束引号
                    lexer.reset_state();
                    return Some(Token::StringEnd);
                }

                Some('\\') => {
                    escape = true;
                    lexer.advance_line(1);
                }

                Some(c) => {
                    value.push(c);
                    escape = false;
                    lexer.advance_line(1);
                }

                None => {
                    // 未闭合字符串
                    lexer.reset_state();
                    return Some(Token::Error);
                }
            }
        }
    }
}

// ================== 错误处理器 ==================
struct ErrorProcessor;
impl StateProcessor for ErrorProcessor {
    fn process(&self, lexer: &mut TokenLexer) -> Option<Token> {
        // 错误恢复逻辑：跳过无效字符
        lexer.advance_line(1);
        lexer.reset_state();
        Some(Token::Error)
    }
}

// ================ 工具方法 ================
impl<'a> TokenLexer<'a> {
    /// 字符预览方法
    pub fn peek_char(&self) -> Option<char> {
        self.source[self.current_byte..].chars().next()
    }

    /// 关键字判断
    fn keyword_or_identifier(&self, ident: &str) -> Token {
        const KEYWORDS: [&str; 9] = [
            "var", "float", "if", "else", "true", "false", "na", "input", "ta",
        ];

        if KEYWORDS.contains(&ident) {
            Token::Keyword
        } else {
            Token::Identifier
        }
    }
}

impl Iterator for TokenLexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.get_next_token()
    }
}

/// A [Token] along with additional metadata
#[derive(Clone, PartialEq, Debug)]
pub struct LexedToken {
    /// The token
    pub token: Token,
    /// The byte positions in the source representing the token
    pub source_bytes: Range<usize>,
    /// The token's span
    pub span: Span,
}

impl LexedToken {
    /// A helper for getting the token's starting line
    pub fn line(&self) -> u32 {
        self.span.start.line
    }

    /// A helper for getting the token's string slice from the source
    pub fn slice<'a>(&self, source: &'a str) -> &'a str {
        &source[self.source_bytes.clone()]
    }
}

impl Default for LexedToken {
    fn default() -> Self {
        Self {
            token: Token::Error,
            source_bytes: Default::default(),
            span: Default::default(),
        }
    }
}

/// The lexer used by the Koto parser
///
/// Wraps a TokenLexer with unbounded lookahead, see peek_n().
#[derive(Clone)]
pub struct TvLexer<'a> {
    lexer: TokenLexer<'a>,
    token_queue: VecDeque<LexedToken>,
}

impl<'a> TvLexer<'a> {
    /// Initializes a lexer with the given input script
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: TokenLexer::new(source),
            token_queue: VecDeque::new(),
        }
    }

    /// Returns the input source
    pub fn source(&self) -> &'a str {
        self.lexer.source
    }

    /// Peeks the nth token that will appear in the output stream
    ///
    /// peek_n(0) is equivalent to calling peek().
    /// peek_n(1) returns the token that will appear after that, and so forth.
    pub fn peek(&mut self, n: usize) -> Option<&LexedToken> {
        let token_queue_len = self.token_queue.len();
        let tokens_to_add = token_queue_len + 1 - n.max(token_queue_len);

        for _ in 0..tokens_to_add {
            if let Some(next) = self.next_token() {
                self.token_queue.push_back(next);
            } else {
                break;
            }
        }

        self.token_queue.get(n)
    }

    fn next_token(&mut self) -> Option<LexedToken> {
        self.lexer.start_token();
        self.lexer.next().map(|token| LexedToken {
            token,
            source_bytes: self.lexer.current_token_bytes(),
            span: self.lexer.span,
        })
    }
}

impl Iterator for TvLexer<'_> {
    type Item = LexedToken;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.token_queue.pop_front() {
            Some(next)
        } else {
            self.next_token()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod lexer_output {
        use super::{Token::*, *};

        fn check_lexer_output(source: &str, tokens: &[(Token, Option<&str>, u32)]) {
            let mut lex = TvLexer::new(source);
            for (i, (token, maybe_slice, line_number)) in tokens.iter().enumerate() {
                let output = lex.next().expect("Expected token");
                assert_eq!(*token, output.token, "Token mismatch at position {i}");
                if let Some(slice) = maybe_slice {
                    assert_eq!(
                        *slice,
                        output.slice(source),
                        "Slice mismatch at position {i}"
                    );
                }
                assert_eq!(
                    *line_number,
                    output.line(),
                    "Line number mismatch at position {i}",
                );
            }

            assert_eq!(lex.next(), None);
        }

        /// 调试工具：打印所有 Token 的详细信息
        pub fn print_tokens(source: &str) {
            let mut lexer = TvLexer::new(source);
            println!("\nTokens for source: {:?}", source);
            println!("{:-^50}", " TOKENS ");

            let mut token_count = 0;
            while let Some(token) = lexer.next() {
                let slice = token.slice(source);
                let start_line = token.span.start.line;
                let start_col = token.span.start.column;
                let end_line = token.span.end.line;
                let end_col = token.span.end.column;

                println!(
                    "[L{:<2}:{:<2}→L{:<2}:{:<2}] {:?} {:<12} → {:?}",
                    start_line + 1,
                    start_col + 1,
                    end_line + 1,
                    end_col + 1,
                    format!("Token::{:?}", token.token),
                    format!("({}..{})", token.source_bytes.start, token.source_bytes.end),
                    slice
                );
                token_count += 1;
            }

            println!("{}", "-".repeat(50));
            println!("Total tokens: {}\n", token_count);
        }

        #[test]
        fn empty_input() {
            let source = "";
            let expected_tokens = vec![];
            print_tokens(source);
            check_lexer_output(source, &expected_tokens);
        }

        #[test]
        fn integer() {
            let source = "123 321  234";
            let expected_tokens = vec![
                (Integer, Some("123"), 0),
                (Whitespace, Some(" "), 0),
                (Integer, Some("321"), 0),
                (Whitespace, Some("  "), 0),
                (Integer, Some("234"), 0),
            ];
            print_tokens(source);
            check_lexer_output(source, &expected_tokens);
        }

        #[test]
        fn integer_basic() {
            let source = "0 5 987654321";
            let expected = vec![
                (Integer, Some("0"), 0),
                (Whitespace, Some(" "), 0),
                (Integer, Some("5"), 0),
                (Whitespace, Some(" "), 0),
                (Integer, Some("987654321"), 0),
            ];
            check_lexer_output(source, &expected);
        }

        #[test]
        fn float_basic() {
            let source = "3.14 0.618 123.456";
            let expected = vec![
                (Float, Some("3.14"), 0),
                (Whitespace, Some(" "), 0),
                (Float, Some("0.618"), 0),
                (Whitespace, Some(" "), 0),
                (Float, Some("123.456"), 0),
            ];
            print_tokens(source);
            check_lexer_output(source, &expected);
        }

        #[test]
        fn float() {
            let source = "123 123.3.";
            let expected_tokens = vec![
                (Integer, Some("123"), 0),
                (Whitespace, Some(" "), 0),
                (Float, Some("123.3"), 0),
                (Error, Some("."), 0),
            ];
            print_tokens(source);
            check_lexer_output(source, &expected_tokens);
        }

        #[test]
        fn mixed_numbers() {
            let source = "42 3.14 100 2.718";
            let expected = vec![
                (Integer, Some("42"), 0),
                (Whitespace, Some(" "), 0),
                (Float, Some("3.14"), 0),
                (Whitespace, Some(" "), 0),
                (Integer, Some("100"), 0),
                (Whitespace, Some(" "), 0),
                (Float, Some("2.718"), 0),
            ];
            check_lexer_output(source, &expected);
        }

        #[test]
        fn edge_cases() {
            let source = "0.0 999.999 ."; // 最后一个点号是非法
            let expected = vec![
                (Float, Some("0.0"), 0),
                (Whitespace, Some(" "), 0),
                (Float, Some("999.999"), 0),
                (Whitespace, Some(" "), 0),
                (Error, Some("."), 0), // 单独的点号报错
            ];
            check_lexer_output(source, &expected);
        }
    }
}
