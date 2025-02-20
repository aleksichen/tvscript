# 词法分析

## 语法特性

| 语法类型 | 示例/规则                                             |
| -------- | ----------------------------------------------------- |
| 变量声明 | `var x = 10`、`var y = close`                         |
| 运算符   | 单字符：`=`, `>`, `!`；多字符：`==`, `!=`, `>=`, `:=` |
| 条件语句 | `if (condition) ... else ...`                         |
| 注释     | 单行注释：`//`，多行注释：`/* ... */`                 |
| 数值类型 | 整数：`42`，浮点数：`3.14`，科学计数法：`1e5`         |
| 字符串   | 双引号包裹：`"Hello, Pine!"`                          |
| 函数调用 | `ta.sma(close, 20)`                                   |
| 关键字   | `var`, `if`, `else`, `for`, `true`, `false`, `na`     |

## Token 设计

| Token 类型  | 描述                 | 示例                                 |
| ----------- | -------------------- | ------------------------------------ |
| Keyword     | 语言关键字           | `var`, `if`, `else`, `true`, `false` |
| Identifier  | 变量名或函数名       | `x`, `close`, `ta.sma`               |
| Operator    | 单字符或多字符运算符 | `=`, `==`, `!=`, `>=`, `:=`          |
| Literal     | 数值或字符串字面量   | `42`, `3.14`, `"text"`               |
| Punctuation | 标点符号             | `(`, `)`, `{`, `}`, `,`              |
| Comment     | 单行或多行注释       | `// Comment`, `/* Multi-line */`     |
| Whitespace  | 空格、制表符、换行符 | ` `, `\t`, `\n`                      |

## 状态机

```text
@startuml

state Initial
state InInteger
state InFloat
state InIdentifier
state InOperator
state InPossibleOp
state InSlash
state InCommentSingle
state InCommentMulti
state InString

[*] --> Initial

' 数字处理
Initial --> InInteger: [0-9]
InInteger --> InInteger: [0-9]
InInteger --> InFloat: .
InInteger --> Initial: 非数字或. (生成 Integer Token)
InFloat --> InFloat: [0-9]
InFloat --> Initial: 非数字 (生成 Float Token)

' 标识符处理
Initial --> InIdentifier: [a-zA-Z_]
InIdentifier --> InIdentifier: [a-zA-Z0-9_.]
InIdentifier --> Initial: 非标识符字符 (生成 Identifier Token)

' 运算符处理
Initial --> InOperator: [=><!+-*/%?:]
InOperator --> InPossibleOp: 允许组合字符（如 = 后跟 =）
InOperator --> Initial: 无法组合 (生成单字符 Operator Token)
InPossibleOp --> Initial: 合法组合 (生成多字符 Operator Token)
InPossibleOp --> Initial: 非法组合 (回退生成单字符 Token)

' 注释处理
Initial --> InSlash: /
InSlash --> InCommentSingle: 第二个 /
InSlash --> InCommentMulti: *
InSlash --> Initial: 其他字符 (生成 / Operator Token)
InCommentSingle --> InCommentSingle: 非换行符
InCommentSingle --> Initial: 换行符 (生成 Comment Token)
InCommentMulti --> InCommentMulti: 非 */
InCommentMulti --> InCommentMulti: * (但非 /)
InCommentMulti --> Initial: */ (生成 Comment Token)

' 字符串处理
Initial --> InString: "
InString --> InString: 非 " 字符
InString --> Initial: " (生成 String Token)

@enduml
```

## 用例推演

| 输入代码                  | 状态流转                                                                                     | 生成 Token 序列                                                                   |
| ------------------------- | -------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------- |
| `var x = 3.14`            | Initial → InIdentifier → Initial → InOperator → Initial → InFloat → Initial                  | Keyword("var"), Identifier("x"), Operator("="), Float("3.14")                     |
| `if close >= 100`         | Initial → InIdentifier → Initial → InOperator → InPossibleOp → Initial → InInteger → Initial | Keyword("if"), Identifier("close"), Operator(">="), Integer("100")                |
| `// This is a comment`    | Initial → InSlash → InCommentSingle → Initial                                                | Comment("// This is a comment")                                                   |
| `str := "Hello"`          | Initial → InIdentifier → Initial → InOperator → InPossibleOp → Initial → InString → Initial  | Identifier("str"), Operator(":="), String("Hello")                                |
| `plot(ta.sma(close, 20))` | Initial → InIdentifier → Initial → Punctuation("(") → ...                                    | Identifier("plot"), Punctuation("("), Identifier("ta.sma"), Punctuation("("), ... |
