use super::state::State;
use crate::lexer::{ TokenLexer, Token };

pub trait TransitionHandler {
    fn transition(&mut self, new_state: State) -> Option<Token>;
    fn transition_to_float(&mut self, int_part: String) -> Option<Token>;
}

impl TransitionHandler for TokenLexer<'_> {
    fn transition(&mut self, new_state: State) -> Option<Token> {
        self.state = new_state;
        self.get_next_token()
    }

    /// 处理整数到浮点数的状态转换
    fn transition_to_float(&mut self, int_part: String) -> Option<Token> {
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
