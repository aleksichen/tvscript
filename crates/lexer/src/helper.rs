pub fn validate_float(s: &str) -> bool {
  // 分离科学计数法和普通浮点
  if let Some(parts) = s.split_once(&['e', 'E'][..]) {
      // 科学计数法分支（原有逻辑）
      validate_scientific_notation(parts.0, parts.1)
  } else {
      // 普通浮点数分支
      validate_normal_float(s)
  }
}

/// 普通浮点数验证
pub fn validate_normal_float(s: &str) -> bool {
  let parts: Vec<&str> = s.split('.').collect();
  parts.len() == 2 && // 必须且只能有一个小数点
  !parts[0].is_empty() && // 不允许 .123 格式（根据Pine Script规范）
  parts[0].chars().all(|c| c.is_ascii_digit()) &&
  !parts[1].is_empty() && 
  parts[1].chars().all(|c| c.is_ascii_digit())
}

/// 科学计数法验证（调整后）
pub fn validate_scientific_notation(mantissa: &str, exponent: &str) -> bool {
  validate_normal_float(mantissa) && // 复用普通浮点验证
  validate_exponent(exponent)
}

/// 指数部分验证
pub fn validate_exponent(s: &str) -> bool {
  let (_sign, digits) = match s.chars().next() {
      Some('+' | '-') => s.split_at(1),
      _ => ("", s),
  };
  !digits.is_empty() && 
  digits.chars().all(|c| c.is_ascii_digit())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_scientific_notation() {
        // 有效格式测试（拆分尾数和指数）
        assert!(validate_scientific_notation("1.23", "4"));
        assert!(validate_scientific_notation("0.123", "-10"));
        assert!(validate_scientific_notation("123.456", "+789"));
        assert!(validate_scientific_notation("9.87654321", "123"));
    }

    #[test]
    fn test_invalid_scientific_notation() {
        // 无效格式测试（拆分尾数和指数）
        assert!(!validate_scientific_notation("1.2.3", "4")); // mantissa不合法
        assert!(!validate_scientific_notation("1.23", "")); // 缺少指数
        assert!(!validate_scientific_notation("1.23", "4.5")); // 指数包含小数点
        assert!(!validate_scientific_notation("abc.def", "123")); // 无效字符
        assert!(!validate_scientific_notation("1.23", "+")); // 只有符号
    }

    #[test]
    fn test_edge_cases() {
        // 需要先拆分字符串
        assert!(!validate_scientific_notation("", "123")); // 空尾数
        assert!(!validate_scientific_notation("1.23", "123e456")); // 指数包含e
    }

    #[test]
    fn test_full_float_validation() {
        // 完整浮点验证（调用validate_float）
        assert!(validate_float("1.23e4")); // 自动拆分尾数1.23和指数4
        assert!(validate_float("123.456"));
        assert!(!validate_float("1.2.3e4")); // 无效尾数
        assert!(!validate_float("123"));     // 普通整数
    }
    
}