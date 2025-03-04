#[derive(Clone, PartialEq, Eq, Debug)]
pub enum State {
    Initial,
    InInteger,
    InFloat,
    InIdentifier,
    InSlash,
    InAssign,
    InOperator,
    InComment,
    StringLiteral,
}

#[derive(Clone, PartialEq)]
pub enum FloatPhase {
    Mantissa,       // 处理尾数部分（必须包含小数点）
    ExponentMarker, // 遇到e/E
    ExponentSign,   // 处理指数符号
    ExponentDigit,  // 处理指数数字
}
