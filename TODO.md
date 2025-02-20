# Rust Pine 开发路线图

## 阶段 1: 词法分析器 (Lexer)

### 迭代 1: 基础词法分析
- [ ] 使用EBNF定义基础语法规则
- [ ] 实现基础Token类型（Identifier, Number, String）
- [ ] 处理空白字符和注释
- [ ] 处理数字字面量（整数、浮点数）
- [ ] 处理基础运算符（+, -, *, /）
- [ ] 实现位置跟踪（用于错误报告）
- [ ] 编写基础单元测试

### 迭代 2: 增强词法分析
- [ ] 处理复杂运算符（?:, =>, ==, !=, >=, <=）
- [ ] 处理字符串插值
- [ ] 处理多行字符串
- [ ] 处理科学计数法数字
- [ ] 添加位置信息的详细跟踪
- [ ] 扩展单元测试

### 迭代 3: 完整词法分析
- [ ] 处理所有PineScript v5关键字
- [ ] 处理所有PineScript v5运算符
- [ ] 处理所有PineScript v5标点符号
- [ ] 实现完整的错误恢复机制
- [ ] 编写完整的单元测试套件

### EBNF 语法设计
```ebnf
Program        = { Statement } ;
Statement      = Declaration | Expression ;
Declaration    = VariableDecl | FunctionDecl ;
VariableDecl   = "var" Identifier "=" Expression ;
FunctionDecl   = Identifier "(" [ ParameterList ] ")" "=>" Expression ;
ParameterList  = Identifier { "," Identifier } ;
Expression     = Literal | Identifier | BinaryExpr | UnaryExpr | CallExpr ;
BinaryExpr     = Expression Operator Expression ;
UnaryExpr      = Operator Expression ;
CallExpr       = Identifier "(" [ ArgumentList ] ")" ;
ArgumentList   = Expression { "," Expression } ;
Literal        = Number | String | Boolean ;
Number         = Digit { Digit } [ "." Digit { Digit } ] ;
String         = '"' { Character } '"' ;
Boolean        = "true" | "false" ;
Identifier     = Letter { Letter | Digit | "_" } ;
Operator       = "+" | "-" | "*" | "/" | "==" | "!=" | ">" | "<" | ">=" | "<=" ;
```

## 阶段 2: 语法分析器 (Parser)
- [ ] 定义PineScript的语法规则
- [ ] 实现AST节点类型
- [ ] 解析表达式（算术、逻辑、比较等）
- [ ] 解析变量声明
- [ ] 解析函数定义
- [ ] 解析控制流语句（if/else, for, while）
- [ ] 解析指标和策略定义
- [ ] 错误恢复机制
- [ ] 编写单元测试

## 阶段 3: 语义分析
- [ ] 实现符号表
- [ ] 类型检查系统
- [ ] 变量作用域解析
- [ ] 函数重载解析
- [ ] 内置函数和指标注册
- [ ] 常量传播
- [ ] 编写单元测试

## 阶段 4: 中间表示 (IR)
- [ ] 设计中间表示形式
- [ ] AST到IR转换
- [ ] 实现基本优化（常量折叠、死代码消除等）
- [ ] 编写单元测试

## 阶段 5: 运行时环境
- [ ] 设计执行引擎架构
- [ ] 实现K线数据处理
- [ ] 实现内置函数
- [ ] 实现绘图功能
- [ ] 实现策略回测框架
- [ ] 编写单元测试

## 阶段 6: 标准库
- [ ] 实现常用技术指标（SMA, EMA, RSI等）
- [ ] 实现常用绘图函数
- [ ] 实现实用工具函数
- [ ] 编写文档和示例

## 阶段 7: 工具链
- [ ] 实现REPL环境
- [ ] 实现格式化工具
- [ ] 实现Linter
- [ ] 实现性能分析工具

## 阶段 8: 文档和发布
- [ ] 编写用户文档
- [ ] 编写开发者指南
- [ ] 设置CI/CD管道
- [ ] 发布到crates.io
- [ ] 编写教程和示例代码