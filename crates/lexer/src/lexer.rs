use std::{collections::VecDeque, ops::Range};

use crate::{
    fsm::{
        processor::{
            CommentProcessor, FloatProcessor, IdentifierProcessor, InAssignProcessor,
            InitialProcessor, IntegerProcessor, OperatorProcessor, SlashProcessor, StringProcessor,
        },
        state::FloatPhase,
        *,
    },
    span::{Position, Span},
};

/// the tokens thats can emerge from the lexer
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[allow(missing_docs)]
pub enum Token {
    Error,
    Whitespace,
    NewLine,
    Comment,
    Keyword,       // 关键字: var, float, if等
    Identifier,    // 标识符
    Operator,      // 运算符
    StringLiteral, // 字符串
    Integer,       // 整数
    Float,         // 浮点数
    // operators
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    Assign,          // =
    AddAssign,       // +=
    SubtractAssign,  // -=
    MultiplyAssign,  // *=
    DivideAssign,    // /=
    RemainderAssign, // %=
    Equal,           // ==
    NotEqual,        // !=
    Greater,         // >
    GreaterOrEqual,  // >=
    Less,            // <
    LessOrEqual,     // <=
    Not,
    // 符号
    Colon,
    Comma,
    Dot,
    Function,
    RoundOpen,
    RoundClose,
    SquareOpen,
    SquareClose,
    CurlyOpen,
    CurlyClose,
    QuestionMark,

    // keywords
    Var,
    If,
    Else,
    ElseIf,
    EOF, // 文件结束符
}

#[derive(Clone, Copy, Debug)]
struct StateSnapshot {
    byte_pos: usize,
    span: Span,
}

#[derive(Clone, Debug)]
pub struct TokenLexer<'a> {
    eof_generated: bool,
    // 状态
    pub state: State,
    // 原始输入
    pub source: &'a str,
    // 当前字节位置
    pub current_byte: usize,
    // 当前Token的起始字节位置
    pub token_start_byte: usize,
    previous_byte: usize,
    // 存储中间值
    pub partial_number: Option<String>,
    // 当前 Token 的字节范围
    span: Span,
    // 环形缓冲区
    state_stack: [Option<StateSnapshot>; 5],
    // 当前栈顶指针
    stack_ptr: usize,
}

impl<'a> TokenLexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            eof_generated: false,
            state: State::Initial,
            source,
            previous_byte: 0,
            current_byte: 0,
            token_start_byte: 0,
            partial_number: None,
            span: Span::default(),
            state_stack: [None, None, None, None, None],
            stack_ptr: 0,
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
        // 保存当前状态到栈（环形缓冲）
        self.state_stack[self.stack_ptr] = Some(StateSnapshot {
            byte_pos: self.current_byte,
            span: self.span,
        });
        self.stack_ptr = (self.stack_ptr + 1) % 5; // 固定容量5

        self.previous_byte = self.current_byte;
        self.current_byte += char_bytes;

        self.span = Span {
            start: self.span.end,
            end: position,
        };
    }

    /// 回退到最近一次记录的状态
    pub fn rollback_line(&mut self) {
        // 计算有效栈位置
        let last_ptr = (self.stack_ptr + 4) % 5;

        if let Some(snapshot) = self.state_stack[last_ptr].take() {
            // 恢复状态
            self.current_byte = snapshot.byte_pos;
            self.span = snapshot.span;

            // 更新栈指针
            self.stack_ptr = last_ptr;
        }
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

    pub fn consume_symbol(&mut self, remaining: &str) -> Option<Token> {
        use Token::*;

        macro_rules! check_symbol {
            ($token_str:expr, $token:ident) => {
                if remaining.starts_with($token_str) {
                    self.advance_line($token_str.len());
                    return Some($token);
                }
            };
        }

        check_symbol!(":", Colon);
        check_symbol!(",", Comma);
        check_symbol!(".", Dot);
        check_symbol!("?", QuestionMark);
        check_symbol!("(", RoundOpen);
        check_symbol!(")", RoundClose);
        check_symbol!("|", Function);
        check_symbol!("[", SquareOpen);
        check_symbol!("]", SquareClose);
        check_symbol!("{", CurlyOpen);
        check_symbol!("}", CurlyClose);
        check_symbol!("!", Not);

        None
    }

    pub fn get_next_token(&mut self) -> Token {
        // 处理 EOF 的边界条件
        if self.current_byte >= self.source.len() {
            return Token::EOF;
        }

        let processor: Box<dyn StateProcessor> = match self.state {
            State::Initial => Box::new(InitialProcessor),
            State::InInteger => Box::new(IntegerProcessor),
            State::InFloat => Box::new(FloatProcessor {
                phase: FloatPhase::Mantissa,
            }),
            State::InIdentifier => Box::new(IdentifierProcessor),
            State::InSlash => Box::new(SlashProcessor),
            State::InOperator => Box::new(OperatorProcessor),
            State::InAssign => Box::new(InAssignProcessor),
            State::InComment => Box::new(CommentProcessor),
            State::StringLiteral => Box::new(StringProcessor),
            // _ => Box::new(ErrorProcessor),
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
    pub fn transition(&mut self, new_state: State) -> Token {
        self.state = new_state;
        self.get_next_token()
    }

    /// 处理整数到浮点数的状态转换
    pub fn transition_to_float(&mut self, int_part: String) -> Token {
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

// ================ 工具方法 ================
impl<'a> TokenLexer<'a> {
    /// 字符预览方法
    pub fn peek_char(&self) -> Option<char> {
        self.source[self.current_byte..].chars().next()
    }

    /// 关键字判断
    pub fn keyword_or_identifier(&self, ident: &str) -> Token {
        match ident {
            "var" => Token::Var,
            "if" => Token::If,
            "else" => Token::Else,
            _ => Token::Identifier,
        }
    }
}

impl Iterator for TokenLexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let token = self.get_next_token();
        match token {
            Token::EOF => {
                self.eof_generated = true;
                None
            }
            _ => Some(token),
        }
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

/// The lexer used by the pine parser
///
/// Wraps a TokenLexer with unbounded lookahead, see peek_n().
#[derive(Clone, Debug)]
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

    /// 根据已经解析好的token信息获取该token的原始输入
    pub fn token_source(&self, token: LexedToken) -> &'a str {
        let start = token.source_bytes.start;
        let end = token.source_bytes.end;

        &self.lexer.source[start..end]
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
        // 已生成EOF时直接返回None
        if self.lexer.eof_generated {
            return None;
        }

        self.lexer.start_token();
        let token = self.lexer.get_next_token();

        // 构造LexedToken
        let lexed_token = LexedToken {
            token,
            source_bytes: self.lexer.current_token_bytes(),
            span: self.lexer.span,
        };

        // 检测EOF标记
        if token == Token::EOF {
            self.lexer.eof_generated = true;
        }
        Some(lexed_token)
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
        fn stack_rollback() {
            let source = "//@version=5";
            let mut lexer = TokenLexer::new(source);

            // 模拟处理流程
            lexer.advance_line(2); // 处理 //
            lexer.advance_line(1); // 处理 @

            // 回退两次
            lexer.rollback_line(); // 回到@之前
            lexer.rollback_line(); // 回到初始位置

            assert_eq!(lexer.current_byte, 0);
            assert_eq!(lexer.span.end.column, 0);
        }

        #[test]
        fn stack_overflow() {
            let mut lexer = TokenLexer::new("test");

            // 连续推进6次（超过栈容量5）
            for _ in 0..6 {
                lexer.advance_line(1);
            }

            // 回退5次应该回到第1次推进的位置
            for _ in 0..5 {
                lexer.rollback_line();
            }

            assert_eq!(lexer.current_byte, 1); // 第1次推进后的位置
        }

        #[test]
        fn basic_rollback() {
            let source = "//comment";
            let mut lexer = TokenLexer::new(source);

            // 模拟处理两个斜杠
            lexer.advance_line(1); // 位置1
            lexer.advance_line(1); // 位置2

            // 回退两次
            lexer.rollback_line(); // 位置1
            lexer.rollback_line(); // 位置0

            assert_eq!(lexer.current_byte, 0);
            assert_eq!(lexer.span.end.column, 0);
        }

        #[test]
        fn rollback_with_newline() {
            let source = "//\nvar";
            let mut lexer = TokenLexer::new(source);

            // 处理到换行符
            lexer.advance_line(2); // 处理 //
            lexer.advance_line(1); // 处理 \n（此时在行1列0）

            // 回退到行尾前
            lexer.rollback_line(); // 回到行0列2
            assert_eq!(lexer.span.end.line, 0);
            assert_eq!(lexer.span.end.column, 2);
        }

        #[test]
        fn empty_input() {
            let source = "";
            let expected_tokens = vec![(Token::EOF, None, 0)];
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
                (Token::EOF, None, 0),
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
                (Token::EOF, None, 0),
            ];
            print_tokens(source);
            check_lexer_output(source, &expected);
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
                (Token::EOF, None, 0),
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
                (Token::EOF, None, 0),
            ];
            check_lexer_output(source, &expected);
        }

        #[test]
        fn test_comment() {
            let source = "// comment";
            print_tokens(source);
            let expected = vec![(Comment, Some("// comment"), 0), (EOF, None, 0)];
            check_lexer_output(source, &expected);
        }

        #[test]
        fn test_combine_slash() {
            let source = "/ /= // test";
            print_tokens(source);
            let expected = vec![
                (Operator, Some("/"), 0),
                (Whitespace, Some(" "), 0),
                (DivideAssign, Some("/="), 0),
                (Whitespace, Some(" "), 0),
                (Comment, Some("// test"), 0),
                (Token::EOF, None, 0),
            ];
            check_lexer_output(source, &expected);
        }

        #[test]
        fn test_operator() {
            let source = ": == != >= <= > < := = += -= *= /= %=";
            print_tokens(source);
        }

        #[test]
        fn test_string() {
            let source = r#"
var a = 123.2
var b = a
var str = "hello world" // 这是一个字符串
"#;
            print_tokens(source);
        }

        #[test]
        fn test_single_slash() {
            let source = "\n// asd\n 123.2 asd123 var if 1 + 1";
            print_tokens(source);
        }

        #[test]
        fn test_var_declare() {
            let source = r#"
   var a = 123
"#;
            print_tokens(source);
        }

        #[test]
        fn test_function_call() {
            let source = r#"
   plot()
"#;
            print_tokens(source);
        }

        #[test]
        fn test_script_1() {
            let source = r#"
//@version=5
strategy("MA Cross", overlay=true)
shortMa = ta.sma(close, 10)
longMa = ta.sma(close, 30)
if ta.crossover(shortMa, longMa)
    strategy.entry("Long", strategy.long, 100)
if ta.crossunder(shortMa, longMa)
    strategy.close("Long")
plot(shortMa, "Short MA")
            "#;
            print_tokens(source);
        }
    }
}
