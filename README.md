# Rust Pine

[![Crates.io](https://img.shields.io/crates/v/rust-pine)](https://crates.io/crates/rust-pine)
[![Documentation](https://docs.rs/rust-pine/badge.svg)](https://docs.rs/rust-pine)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)

基于Rust实现的TradingView PineScript解析器，提供高性能、类型安全的PineScript解析和执行环境。

## 功能特点

- 完整的PineScript语法解析
- 内置常用技术指标库
- 支持自定义指标和策略
- 高性能执行引擎
- 跨平台支持（Windows, macOS, Linux）

## 安装

在`Cargo.toml`中添加依赖：

```toml
[dependencies]
rust-pine = "0.1"
```

## 使用案例

### 解析PineScript

```rust
use rust_pine::parser::Parser;
use rust_pine::ast::Script;

let pine_code = r#"
//@version=5
indicator("My Script", overlay=true)
plot(close)
"#;

let parser = Parser::new();
let script: Script = parser.parse(pine_code)?;
```

### 执行策略

```rust
use rust_pine::runtime::Runtime;
use rust_pine::data::Bar;

let mut runtime = Runtime::new();
runtime.execute(&script);

let bars = vec![
    Bar::new(/* 填充K线数据 */),
    // ...
];

for bar in bars {
    runtime.on_bar(&bar);
}
```

## 贡献指南

欢迎贡献代码！请遵循以下步骤：

1. Fork 本项目
2. 创建特性分支 (`git checkout -b feature/AmazingFeature`)
3. 提交更改 (`git commit -m 'Add some AmazingFeature'`)
4. 推送到分支 (`git push origin feature/AmazingFeature`)
5. 打开 Pull Request

## 许可证

本项目采用 MIT 许可证 - 详情请见 [LICENSE](LICENSE) 文件。

## 致谢

- TradingView 提供的 PineScript 语言
- Rust 社区提供的优秀工具链
