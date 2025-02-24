use super::state::{self, FloatPhase, State};
use crate::{
    helper::validate_float,
    lexer::{Token, TokenLexer},
};

pub trait StateProcessor {
    fn process(&self, lexer: &mut TokenLexer) -> Token;
}

/// 初始状态处理器
pub struct InitialProcessor;
impl StateProcessor for InitialProcessor {
    fn process(&self, lexer: &mut TokenLexer) -> Token {
        let c: char = match lexer.peek_char() {
            Some(c) => c,
            None => return Token::EOF,
        };
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
                lexer.transition(State::StringLiteral)
            }
            // 空格
            ' ' | '\t' => {
                lexer.consume_whitespace();
                Token::Whitespace
            }
            // 换行处理
            '\n' | '\r' => lexer.consume_newline(),
            // 其他符号处理
            _ => {
                // 获取剩余字符, 如果没有字符则返回EOF
                let result = match lexer.source.get(lexer.current_byte..) {
                    Some(c) => lexer.consume_symbol(c),
                    None => Some(Token::EOF),
                };
                
                result.unwrap()
            }
        }
    }
}

/// 整数状态处理器
pub struct IntegerProcessor;
impl StateProcessor for IntegerProcessor {
    fn process(&self, lexer: &mut TokenLexer) -> Token {
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
        Token::Integer
    }
}

/// 浮点数处理器
pub struct FloatProcessor {
    pub(crate) phase: state::FloatPhase,
}

impl StateProcessor for FloatProcessor {
    fn process(&self, lexer: &mut TokenLexer) -> Token {
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
        if valid {
            Token::Float
        } else {
            Token::Error
        }
    }
}

/// 标识符处理器
pub struct IdentifierProcessor;
impl StateProcessor for IdentifierProcessor {
    fn process(&self, lexer: &mut TokenLexer) -> Token {
        let mut ident = String::new();
        // 解析后续的字符
        while let Some(c) = lexer.peek_char() {
            if c.is_alphanumeric() || c == '_' {
                ident.push(c);
                lexer.advance_line(1)
            } else {
                break;
            }
        }
        lexer.reset_state();
        lexer.keyword_or_identifier(&ident)
    }
}

// ================== 斜杠处理器 ==================
pub struct SlashProcessor;
impl StateProcessor for SlashProcessor {
    fn process(&self, lexer: &mut TokenLexer) -> Token {
        // 消费一个 /
        lexer.advance_line(1);
        // 处理可能的EOF情况
        let next_char = match lexer.peek_char() {
            Some(c) => c,
            None => {
                // 场景：输入为单个"/"结尾
                // 示例代码：x = 10 /
                lexer.reset_state();
                return Token::Operator; // 返回有效的除法运算符
            }
        };
        match next_char {
            // 遇到第二个斜杠 -> 单行注释
            '/' => {
                lexer.advance_line(1);
                lexer.transition(State::InComment)
            }

            // 其他情况 -> 普通除法运算符
            _ => {
                lexer.reset_state();
                Token::Operator
            }
        }
    }
}

pub struct InAssignProcessor;
impl StateProcessor for InAssignProcessor {
    fn process(&self, lexer: &mut TokenLexer) -> Token {
        lexer.advance_line(1);
        lexer.reset_state();
        return Token::Assign;
    }
}

/// ================== 运算符处理器 ==================
pub struct OperatorProcessor;
impl StateProcessor for OperatorProcessor {
    fn process(&self, lexer: &mut TokenLexer) -> Token {
        // 获取下一个字符
        let next_char = match lexer.peek_char() {
            Some(c) => c,
            None => return Token::Operator,
        };

        let mut operator = String::from(next_char);
        lexer.advance_line(1);

        // 如果当前字符是 = , 并且下一个字符不是= 直接transition到State::InAssign状态
        if next_char == '=' {
            if let Some(next_c) = lexer.peek_char() {
                if next_c != '=' {
                    // 回退一个字符, 并且跳转assign状态
                    lexer.rollback_line();
                    return lexer.transition(State::InAssign);
                }
            }
        }
        // 检查可能的多字符运算符
        if let Some(next_c) = lexer.source[lexer.current_byte..].chars().next() {
            let combined = match (next_char, next_c) {
                ('+', '=') => "+=",
                ('-', '=') => "-=",
                ('*', '=') => "*=",
                ('/', '=') => "/=",
                ('%', '=') => "%=",

                ('!', '=') => "!=",
                ('>', '=') => ">=",
                ('<', '=') => "<=",
                ('+', '+') => "++",
                _ => "",
            };

            if !combined.is_empty() {
                operator.push(next_c);
                lexer.advance_line(1);
            }
        }

        lexer.reset_state();
        Token::Operator
    }
}

// ================== 单行注释处理器 ==================
pub struct CommentProcessor;
impl StateProcessor for CommentProcessor {
    fn process(&self, lexer: &mut TokenLexer) -> Token {
        // 回退两步获取完整注释起始位置
        lexer.rollback_line(); // 回退到第二个斜杠前
        lexer.rollback_line(); // 回退到第一个斜杠前

        // 找到行末或文件结尾
        let remaining = &lexer.source[lexer.current_byte..];
        let line_end = remaining.find(['\n', '\r']).unwrap_or(remaining.len());

        // 推进到行末
        lexer.advance_line(line_end);

        // 注释结束恢复到初始状态
        lexer.state = State::Initial;
        // 生成完整注释 Token
        Token::Comment
    }
}

// ================== 字符串处理器 ==================
pub struct StringProcessor;
impl StateProcessor for StringProcessor {
    fn process(&self, lexer: &mut TokenLexer) -> Token {
        let mut value = String::new();
        let mut escape = false;

        loop {
            match lexer.peek_char() {
                Some('"') if !escape => {
                    lexer.advance_line(1); // 消耗结束引号
                    lexer.reset_state();
                    return Token::StringLiteral;
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
                    return Token::Error;
                }
            }
        }
    }
}

// ================== 错误处理器 ==================
pub struct ErrorProcessor;
impl StateProcessor for ErrorProcessor {
    fn process(&self, lexer: &mut TokenLexer) -> Token {
        // 错误恢复逻辑：跳过无效字符
        lexer.advance_line(1);
        lexer.reset_state();
        Token::Error
    }
}
