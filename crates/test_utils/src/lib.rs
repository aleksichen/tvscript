use parser::pratt::PrecedenceLevel;
use parser::pratt::create_pratt_parser;
use parser::core::ParserCoreBuilder;
use lexer::lexer::TvLexer;
use parser::pratt::Expr;


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

pub fn parse_expr(input: &str) -> Expr {
  let mut core = ParserCoreBuilder::build(input);
  core.consume_until_token();
  let mut pratt = create_pratt_parser(&mut core);
  pratt.parse_expression(PrecedenceLevel::Lowest)
}

