use lexer::span::Span;
use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Clone, Debug)]
#[error("{error}")]
pub struct Error {
    /// The error itself
    pub error: ErrorKind,
    /// The span in the source string where the error occurred
    pub span: Span,
}

impl Error {
    /// Initializes a parser error with the specific error type and its associated span
    pub fn new(error: ErrorKind, span: Span) -> Self {
        Self { error, span }
    }
}

#[derive(Error, Clone, Debug)]
#[allow(missing_docs)]
pub enum ErrorKind {
    #[error(transparent)]
    InternalError(#[from] InternalError), // 内部错误
    #[error(transparent)]
    SyntaxError(#[from] SyntaxError),   // 语法错误
    #[error(transparent)]
    StringFormatError(#[from] StringFormatError), // 字符串格式化错误
}

#[derive(Error, Clone, Debug)]
#[allow(missing_docs)]
pub enum InternalError {
    #[error("there are more nodes in the program than the AST can support")]
    AstCapacityOverflow,
    #[error("there are more constants in the program than the runtime can support")]
    ConstantPoolCapacityOverflow,
    #[error("expected ':' after map key")]
    ExpectedMapColon,
    #[error("failed to parse ID")]
    IdParseFailure,
    #[error("failed to parse chain")]
    ChainParseFailure,
    #[error("missing assignment target")]
    MissingAssignmentTarget,
    #[error("frame unavailable during parsing")]
    MissingFrame,
    #[error("failed to parse number")]
    NumberParseFailure,
    #[error("failed to parse raw string")]
    RawStringParseFailure,
    #[error("unexpected token")]
    UnexpectedToken,
}

#[derive(Error, Clone, Debug)]
#[allow(missing_docs)]
pub enum SyntaxError {
    #[error("ascii value out of range, the maximum is \\x7f")]
    AsciiEscapeCodeOutOfRange,
    #[error("expected end of arguments ')'")]
    ExpectedArgsEnd,
    #[error("expected target for assignment")]
    ExpectedAssignmentTarget,
    #[error("expected '=' assignment after meta key")]
    ExpectedAssignmentAfterMetaKey,
    #[error("expected argument for catch expression")]
    ExpectedCatchArgument,
    #[error("expected catch expression after try")]
    ExpectedCatch,
    #[error("expected closing parenthesis ')'")]
    ExpectedCloseParen,
    #[error("all arguments following a default value must also have a default value")]
    ExpectedDefaultValue,
    #[error("expected expression after 'else'.")]
    ExpectedElseExpression,
    #[error("expected condition for 'else if'.")]
    ExpectedElseIfCondition,
    #[error("expected expression")]
    ExpectedExpression,
    #[error("expected arguments in for loop")]
    ExpectedForArgs,
    #[error("expected 'in' keyword in for loop")]
    ExpectedForInKeyword,
    #[error("expected iterable in for loop")]
    ExpectedForIterable,
    #[error("expected format string after ':'")]
    ExpectedFormatString,
    #[error("expected end of function arguments '|'")]
    ExpectedFunctionArgsEnd,
    #[error("expected ID in import expression")]
    ExpectedIdInImportExpression,
    #[error("expected condition after 'if'")]
    ExpectedIfCondition,
    #[error("expected import after from")]
    ExpectedImportAfterFrom,
    #[error("expected module ID in import expression")]
    ExpectedImportModuleId,
    #[error("expected index end ']'")]
    ExpectedIndexEnd,
    #[error("expected index expression")]
    ExpectedIndexExpression,
    #[error("expected id after 'as'")]
    ExpectedIdAfterAs,
    #[error("expected List end ']'")]
    ExpectedListEnd,
    #[error("expected ':' after map key")]
    ExpectedMapColon,
    #[error("expected '}}' at end of map declaration")]
    ExpectedMapEnd,
    #[error("expected map entry")]
    ExpectedMapEntry,
    #[error("expected key after '.' in Map access")]
    ExpectedMapKey,
    #[error("expected value after ':' in Map")]
    ExpectedMapValue,
    #[error("expected expression in match arm")]
    ExpectedMatchArmExpression,
    #[error("expected expression after then in match arm")]
    ExpectedMatchArmExpressionAfterThen,
    #[error("expected condition after if in match arm")]
    ExpectedMatchCondition,
    #[error("expected expression after match")]
    ExpectedMatchExpression,
    #[error("expected pattern for match arm")]
    ExpectedMatchPattern,
    #[error("expected id after @meta")]
    ExpectedMetaId,
    #[error("expected a module path after 'from'")]
    ExpectedPathAfterFrom,
    #[error("expected a line break before starting a map block")]
    ExpectedLineBreakBeforeMapBlock,
    #[error("expected '}}' at end of string placeholder")]
    ExpectedStringPlaceholderEnd,
    #[error("expected expression in switch arm")]
    ExpectedSwitchArmExpression,
    #[error("expected expression after 'then' in switch arm")]
    ExpectedSwitchArmExpressionAfterThen,
    #[error("expected a test name")]
    ExpectedTestName,
    #[error("expected expression after 'then'")]
    ExpectedThenExpression,
    #[error("expected condition in until loop")]
    ExpectedUntilCondition,
    #[error("expected condition in while loop")]
    ExpectedWhileCondition,
    #[error("expected a type after ':'")]
    ExpectedType,
    #[error("non-inline if expression isn't allowed in this context")]
    IfBlockNotAllowedInThisContext,
    #[error("ellipsis found outside of nested match patterns")]
    MatchEllipsisOutsideOfNestedPatterns,
    #[error("'else' can only be used in the last arm in a match expression")]
    MatchElseNotInLastArm,
    #[error("nested types aren't currently supported")]
    NestedTypesArentSupported,
    #[error("keyword reserved for future use")]
    ReservedKeyword,
    #[error("'self' doesn't need to be declared as an argument")]
    SelfArg,
    #[error("'else' can only be used in the last arm in a switch expression")]
    SwitchElseNotInLastArm,
    #[error("unexpected character in numeric escape code")]
    UnexpectedCharInNumericEscapeCode,
    #[error("'.' after imported item. You might want a 'from' import instead")]
    UnexpectedDotAfterImportItem,
    #[error("unexpected escape pattern in string")]
    UnexpectedEscapeInString,
    #[error("unexpected 'else' in match arm")]
    UnexpectedMatchElse,
    #[error("unexpected if condition in match arm")]
    UnexpectedMatchIf,
    #[error("unexpected meta key")]
    UnexpectedMetaKey,
    #[error("unexpected 'else' in switch arm")]
    UnexpectedSwitchElse,
    #[error("unexpected '?'")]
    UnexpectedNullCheck,
    #[error("unexpected token")]
    UnexpectedToken,
    #[error("unicode value out of range, the maximum is \\u{{10ffff}}")]
    UnicodeEscapeCodeOutOfRange,
    #[error("unterminated numeric escape code")]
    UnterminatedNumericEscapeCode,
    #[error("unterminated string")]
    UnterminatedString,
}

#[derive(Error, Clone, Debug)]
#[allow(missing_docs)]
pub enum StringFormatError {
    #[error("expected a number '{0}'")]
    ExpectedNumber(char),
    #[error("{0} is larger than the maximum of {}", u32::MAX)]
    FormatNumberIsTooLarge(u64),
    #[error("an unexpected internal error occurred")]
    InternalError,
    #[error("unexpected token '{0}'")]
    UnexpectedToken(char),
}
