#[derive(Clone, PartialEq, Eq)]
pub enum State {
    Initial,
    InInteger,
    InFloat,
    InIdentifier,
    InSlash,
    InOperator,
    InPossibleOp,
    InComment,
    KeywordOrIdentifier,
    StringLiteral,
    Dot,
}

#[derive(Clone, PartialEq)]
pub enum FloatPhase {
    Mantissa,       // 处理尾数部分（必须包含小数点）
    ExponentMarker, // 遇到e/E
    ExponentSign,   // 处理指数符号
    ExponentDigit,  // 处理指数数字
}
