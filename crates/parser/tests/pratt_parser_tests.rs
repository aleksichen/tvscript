// tests/pratt_parser_tests.rs
use std::{fs, panic::AssertUnwindSafe};
use parser::{error::{Error, ErrorKind}, pratt::Expr};
use serde_json::Value;
use serde::Deserialize;
use similar_asserts::assert_eq;
use test_utils::parse_expr;
// 测试用例目录
const TEST_CASES_DIR: &str = "tests/pratt_test_cases";

// 测试用例结构定义
#[derive(Debug, Deserialize)]
struct TestSuite {
    category: String,
    description: String,
    tests: Vec<TestCase>,
}

#[derive(Debug, Deserialize)]
struct TestCase {
    name: String,
    input: String,
    expected: TestExpectation,
    #[serde(default)]
    pending: bool,
    #[serde(default)]
    tags: Vec<String>,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum TestExpectation {
    Success { ast: Value },
    Error { error: ErrorDetails },
}

#[derive(Debug, Deserialize)]
struct ErrorDetails {
    r#type: String,
    message: String,
    #[serde(default)]
    span: Option<SpanDetails>,
}

#[derive(Debug, Deserialize)]
struct SpanDetails {
    start: usize,
    end: usize,
}

// 测试运行器
fn run_golden_tests() {
    let test_files = fs::read_dir(TEST_CASES_DIR)
        .unwrap()
        .filter_map(|entry| entry.ok())
        .filter(|e| e.path().extension().map(|ext| ext == "json").unwrap_or(false));

    for file in test_files {
        let path = file.path();
        let suite: TestSuite = serde_json::from_reader(fs::File::open(&path).unwrap())
            .unwrap_or_else(|_| panic!("Invalid test suite format: {:?}", path));

        for case in suite.tests {
            if case.pending {
                continue;
            }

            // 为每个测试生成独立模块
            let test_name = format!("{}_{}", suite.category, case.name);
            let test_fn = move || {
                let actual = parse_expr(&case.input);
                verify_expectation(Ok(actual), &case.expected, &case.input);
            };

            // 使用 #[test] 宏注册测试
            std::thread::spawn(move || {
                let test = AssertUnwindSafe(test_fn);
                let result = std::panic::catch_unwind(test);
                assert!(result.is_ok(), "Test case panicked: {}", case.name);
            }).join().unwrap();

        }
    }
}

// 增强版验证逻辑
fn verify_expectation(result: Result<Expr, Error>, expected: &TestExpectation, input: &str) {
    match (&result, expected) {
        (Ok(actual), TestExpectation::Success { ast }) => {
            let expected_ast = parse_expected_ast(ast, input);
            assert_eq!(
                actual,
                &expected_ast,
                "\nInput: {}\nExpected AST:\n{:#?}\nActual AST:\n{:#?}",
                input,
                expected_ast,
                actual
            );
        }
        (Err(err), TestExpectation::Error { error }) => {
            verify_error_details(&err, error, input);
        }
        _ => panic!(
            "Mismatched expectation for input: {}\nExpected: {:#?}\nActual: {:#?}",
            input, expected, result
        ),
    }
}

// AST 对比（需实现 From<Value> for Expr 或自定义转换）
fn parse_expected_ast(value: &Value, input: &str) -> Expr {
    // 这里应实现从 JSON 到 Expr 的反序列化逻辑
    Expr::from_json(value).unwrap_or_else(|_| panic!("Invalid AST spec for input: {}", input))
}

// 错误详情验证
fn verify_error_details(actual: &Error, expected: &ErrorDetails, input: &str) {
    // 类型匹配
    let type_matches = match expected.r#type.as_str() {
        "SyntaxError" => matches!(actual.error, ErrorKind::SyntaxError(_)),
        "InternalError" => matches!(actual.error, ErrorKind::InternalError(_)),
        _ => false,
    };

    assert!(
        type_matches,
        "Error type mismatch for input: {}\nExpected: {}\nActual: {}",
        input, expected.r#type, actual
    );

    // 消息包含检查
    assert!(
        actual.to_string().contains(&expected.message),
        "Error message mismatch for input: {}\nExpected contains: {}\nActual: {}",
        input, expected.message, actual
    );

    // Span 验证
    if let Some(expected_span) = &expected.span {
        let actual_span = actual.span;
        assert_eq!(
            actual_span.start, expected_span.start,
            "Span start mismatch for input: {}",
            input
        );
        assert_eq!(
            actual_span.end, expected_span.end,
            "Span end mismatch for input: {}",
            input
        );
    }
}

// 主测试入口
#[test]
fn golden_tests() {
    run_golden_tests();
}
