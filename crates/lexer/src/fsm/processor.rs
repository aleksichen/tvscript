use super::state::{State, FloatPhase};
use crate::lexer::{ TokenLexer, Token };

pub trait StateProcessor {
    fn process(&self, lexer: &mut TokenLexer) -> Option<Token>;
}

pub struct InitialProcessor;
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

pub struct IntegerProcessor; 