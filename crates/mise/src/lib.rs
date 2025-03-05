/// 处理字符串字面量（支持常见转义字符）
pub fn parse_string_literal(raw: &str) -> Option<String> {
    // 确保至少有两个字符（两个引号）
    if raw.len() < 2 {
        return None;
    }
    
    let quote_char = raw.chars().next()?;
    if !(quote_char == '"' || quote_char == '\'') {
        return None;
    }
    
    // 去除首尾引号
    let content = &raw[1..raw.len()-1];
    
    // 处理转义字符
    let mut result = String::with_capacity(content.len());
    let mut chars = content.chars().peekable();
    
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('\\') => result.push('\\'),
                Some('\"') => result.push('\"'),
                Some('\'') => result.push('\''),
                Some('0') => result.push('\0'),
                // 处理Unicode转义（如\u{1F600}）
                Some('u') => {
                    if chars.next() != Some('{') {
                        return None;
                    }
                    
                    let mut hex_str = String::new();
                    while let Some(&c) = chars.peek() {
                        if c == '}' {
                            chars.next(); // 消费}
                            break;
                        }
                        if c.is_ascii_hexdigit() {
                            hex_str.push(c);
                            chars.next();
                        } else {
                            return None;
                        }
                    }
                    
                    let code_point = u32::from_str_radix(&hex_str, 16).ok()?;
                    result.push(char::from_u32(code_point)?);
                }
                _ => return None, // 无效转义序列
            }
        } else {
            result.push(c);
        }
    }
    
    Some(result)
}