use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    fmt,
    hash::{Hash, Hasher},
    num::TryFromIntError,
    ops::Range,
};

use memory::ptr::Ptr;

use crate::{error::InternalError, string_slice::StringSlice};

/// 常量池索引类型，用于引用常量池中的具体常量
///
/// 实现了与u32/usize之间的安全转换，保证索引的有效性
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ConstantIndex(u32);

// 实现各种类型转换 trait
impl From<ConstantIndex> for u32 {
    fn from(value: ConstantIndex) -> Self {
        value.0
    }
}

impl From<ConstantIndex> for usize {
    fn from(value: ConstantIndex) -> Self {
        value.0 as usize
    }
}

impl From<u32> for ConstantIndex {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl TryFrom<usize> for ConstantIndex {
    type Error = TryFromIntError;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Ok(Self(u32::try_from(value)?))
    }
}

impl fmt::Display for ConstantIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// 常量池条目类型（内部表示）
#[derive(Clone, Debug, PartialEq)]
enum ConstantEntry {
    /// 64位浮点数常量
    F64(f64),
    /// 64位整数常量
    I64(i64),
    /// 字符串常量在字符串数据中的字节范围
    Str(Range<usize>),
}

/// 公开的常量类型表示
#[derive(Clone, Debug, PartialEq)]
pub enum Constant<'a> {
    /// 64位浮点数
    F64(f64),
    /// 64位整数
    I64(i64),
    /// 字符串切片
    Str(&'a str),
}

/// 常量池主结构
///
/// 用于存储和管理脚本编译过程中的所有常量，包括：
/// - 数值常量（f64/i64）
/// - 字符串常量
/// 通过[ConstantPoolBuilder]构建最终实例
#[derive(Clone, Debug)]
pub struct ConstantPool {
    /// 常量条目列表，按添加顺序存储
    constants: Vec<ConstantEntry>,
    /// 所有字符串常量拼接后的存储容器
    /// 使用Ptr<String>优化内存布局（64位系统下8字节指针）
    string_data: Ptr<String>,
    /// 基于常量内容生成的哈希值，用于快速比较池内容
    hash: u64,
}

impl Default for ConstantPool {
    fn default() -> Self {
        Self {
            constants: vec![],
            string_data: String::default().into(),
            hash: 0,
        }
    }
}

impl ConstantPool {
    /// 获取常量池中常量的总数
    pub fn size(&self) -> usize {
        self.constants.len()
    }

    /// 根据索引获取常量值
    ///
    /// # 参数
    /// - index: 常量索引
    ///
    /// # 返回
    /// 返回Option包装的常量值，索引无效时返回None
    pub fn get(&self, index: usize) -> Option<Constant> {
        match self.constants.get(index) {
            Some(constant_info) => match constant_info {
                ConstantEntry::F64(n) => Some(Constant::F64(*n)),
                ConstantEntry::I64(n) => Some(Constant::I64(*n)),
                ConstantEntry::Str(range) => Some(Constant::Str(&self.string_data[range.clone()])),
            },
            None => None,
        }
    }

    /// 获取字符串存储容器的智能指针
    pub fn string_data(&self) -> &Ptr<String> {
        &self.string_data
    }

    #[inline]
    pub fn get_str(&self, index: ConstantIndex) -> &str {
        // Safety: The bounds have already been checked while the pool is being prepared
        unsafe { self.string_data.get_unchecked(self.get_str_bounds(index)) }
    }

    #[inline]
    pub fn get_string_slice(&self, index: ConstantIndex) -> StringSlice<usize> {
        // Safety: The bounds have already been checked while the pool is being prepared
        unsafe { StringSlice::new_unchecked(self.string_data.clone(), self.get_str_bounds(index)) }
    }

    fn get_str_bounds(&self, index: ConstantIndex) -> Range<usize> {
        match self.constants.get(usize::from(index)) {
            Some(ConstantEntry::Str(range)) => range.clone(),
            _ => panic!("Invalid index"),
        }
    }

    /// Returns the f64 corresponding to the provided constant index
    ///
    /// Warning! Panics if there isn't an f64 at the provided index
    pub fn get_f64(&self, index: ConstantIndex) -> f64 {
        match self.constants.get(usize::from(index)) {
            Some(ConstantEntry::F64(n)) => *n,
            _ => panic!("Invalid index"),
        }
    }

    /// Returns the i64 corresponding to the provided constant index
    ///
    /// Warning! Panics if there isn't an i64 at the provided index
    pub fn get_i64(&self, index: ConstantIndex) -> i64 {
        match self.constants.get(usize::from(index)) {
            Some(ConstantEntry::I64(n)) => *n,
            _ => panic!("Invalid index"),
        }
    }

    /// Provides an iterator that iterates over the pool's constants
    pub fn iter(&self) -> ConstantPoolIterator {
        ConstantPoolIterator::new(self)
    }
}

/// An iterator that iterates over a [ConstantPool]'s constants
pub struct ConstantPoolIterator<'a> {
    pool: &'a ConstantPool,
    index: usize,
}

impl<'a> ConstantPoolIterator<'a> {
    fn new(pool: &'a ConstantPool) -> Self {
        Self { pool, index: 0 }
    }
}

impl<'a> Iterator for ConstantPoolIterator<'a> {
    type Item = Constant<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.pool.get(self.index);
        self.index += 1;
        result
    }
}

impl fmt::Display for ConstantPool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, constant) in self.iter().enumerate() {
            write!(f, "{i:<8}")?;
            match constant {
                Constant::F64(n) => write!(f, "Float   {n}")?,
                Constant::I64(n) => write!(f, "Int     {n}")?,
                Constant::Str(s) => write!(f, "String  {s}")?,
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl PartialEq for ConstantPool {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash
    }
}

impl Hash for ConstantPool {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash.hash(state);
    }
}

/// 常量池构建器
///
/// 在解析阶段逐步构建常量池，提供以下功能：
/// 1. 自动去重：相同值的常量只存储一次
/// 2. 增量哈希：实时维护池内容的哈希值
/// 3. 类型安全：保证常量类型的正确存储
#[derive(Default)]
pub(crate) struct ConstantPoolBuilder {
    /// 临时存储的常量条目
    constants: Vec<ConstantEntry>,
    /// 字符串拼接缓冲区
    string_data: String,
    /// 增量哈希计算器
    hasher: DefaultHasher,
    /// 字符串常量去重映射表（字符串内容 -> 索引）
    string_map: HashMap<String, ConstantIndex>,
    /// 浮点数常量去重映射表（二进制表示 -> 索引）
    float_map: HashMap<u64, ConstantIndex>,
    /// 整数常量去重映射表（数值 -> 索引）
    int_map: HashMap<i64, ConstantIndex>,
}

impl ConstantPoolBuilder {
    /// 添加字符串常量到池中
    ///
    /// # 参数
    /// - s: 要添加的字符串
    ///
    /// # 返回
    /// 成功返回ConstantIndex，失败返回InternalError
    ///
    /// # 注意
    /// - 自动去重：相同字符串只存储一次
    /// - 维护哈希：影响最终池的哈希值
    pub fn add_string(&mut self, s: &str) -> Result<ConstantIndex, InternalError> {
        match self.string_map.get(s) {
            Some(index) => Ok(*index),
            None => {
                let result = ConstantIndex::try_from(self.constants.len())
                    .map_err(|_| InternalError::ConstantPoolCapacityOverflow)?;

                let start = self.string_data.len();
                let end = start + s.len();
                self.string_data.push_str(s);
                self.constants.push(ConstantEntry::Str(start..end));
                s.hash(&mut self.hasher);

                self.string_map.insert(s.to_string(), result);

                Ok(result)
            }
        }
    }

    pub fn add_f64(&mut self, n: f64) -> Result<ConstantIndex, InternalError> {
        let n_u64 = n.to_bits();

        match self.float_map.get(&n_u64) {
            Some(index) => Ok(*index),
            None => {
                let result = ConstantIndex::try_from(self.constants.len())
                    .map_err(|_| InternalError::ConstantPoolCapacityOverflow)?;
                self.constants.push(ConstantEntry::F64(n));
                n_u64.hash(&mut self.hasher);
                self.float_map.insert(n_u64, result);
                Ok(result)
            }
        }
    }

    pub fn add_i64(&mut self, n: i64) -> Result<ConstantIndex, InternalError> {
        match self.int_map.get(&n) {
            Some(index) => Ok(*index),
            None => {
                let result = ConstantIndex::try_from(self.constants.len())
                    .map_err(|_| InternalError::ConstantPoolCapacityOverflow)?;
                self.constants.push(ConstantEntry::I64(n));
                n.hash(&mut self.hasher);
                self.int_map.insert(n, result);
                Ok(result)
            }
        }
    }

    pub fn get_str(&self, index: ConstantIndex) -> &str {
        match self.constants.get(usize::from(index)) {
            Some(ConstantEntry::Str(range)) => {
                // Safety: The bounds have already been checked while the pool is being prepared
                unsafe { self.string_data.get_unchecked(range.clone()) }
            }
            _ => panic!("Invalid index"),
        }
    }

    pub fn build(self) -> ConstantPool {
        ConstantPool {
            constants: self.constants,
            string_data: self.string_data.into(),
            hash: self.hasher.finish(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn floats_are_equal(a: f64, b: f64) -> bool {
        (a - b).abs() < f64::EPSILON
    }

    #[test]
    fn test_adding_strings() {
        let mut builder = ConstantPoolBuilder::default();

        let s1 = "test";
        let s2 = "test2";

        // 1 byte for string length
        assert_eq!(ConstantIndex(0), builder.add_string(s1).unwrap());
        assert_eq!(ConstantIndex(1), builder.add_string(s2).unwrap());

        // Don't duplicate string_data
        assert_eq!(ConstantIndex(0), builder.add_string(s1).unwrap());
        assert_eq!(ConstantIndex(1), builder.add_string(s2).unwrap());

        let pool = builder.build();

        assert_eq!(s1, pool.get_str(0.into()));
        assert_eq!(s2, pool.get_str(1.into()));

        assert_eq!(2, pool.size());
    }

    #[test]
    fn test_adding_numbers() {
        let mut builder = ConstantPoolBuilder::default();

        let n1 = 3;
        let n2 = 9.87654321;

        assert_eq!(ConstantIndex(0), builder.add_i64(n1).unwrap());
        assert_eq!(ConstantIndex(1), builder.add_f64(n2).unwrap());

        // Don't duplicate numbers
        assert_eq!(ConstantIndex(0), builder.add_i64(n1).unwrap());
        assert_eq!(ConstantIndex(1), builder.add_f64(n2).unwrap());

        let pool = builder.build();

        assert_eq!(n1, pool.get_i64(0.into()));
        assert!(floats_are_equal(n2, pool.get_f64(1.into())));

        assert_eq!(2, pool.size());
    }

    #[test]
    fn test_adding_numbers_and_strings() {
        let mut builder = ConstantPoolBuilder::default();

        let n1 = -1.1;
        let n2 = 99;
        let s1 = "O_o";
        let s2 = "^_^";

        assert_eq!(ConstantIndex(0), builder.add_f64(n1).unwrap());
        assert_eq!(ConstantIndex(1), builder.add_string(s1).unwrap());
        assert_eq!(ConstantIndex(2), builder.add_i64(n2).unwrap());
        assert_eq!(ConstantIndex(3), builder.add_string(s2).unwrap());

        let pool = builder.build();

        assert!(floats_are_equal(n1, pool.get_f64(0.into())));
        assert_eq!(s1, pool.get_str(1.into()));
        assert_eq!(n2, pool.get_i64(2.into()));
        assert_eq!(s2, pool.get_str(3.into()));

        assert_eq!(4, pool.size());
    }

    #[test]
    fn test_iter() {
        let mut builder = ConstantPoolBuilder::default();

        let n1 = -1;
        let n2 = 99.9;
        let s1 = "O_o";
        let s2 = "^_^";

        builder.add_i64(n1).unwrap();
        builder.add_string(s1).unwrap();
        builder.add_f64(n2).unwrap();
        builder.add_string(s2).unwrap();

        let pool = builder.build();

        let mut iter = pool.iter();
        assert_eq!(iter.next(), Some(Constant::I64(-1)));
        assert_eq!(iter.next(), Some(Constant::Str("O_o")));
        assert_eq!(iter.next(), Some(Constant::F64(99.9)));
        assert_eq!(iter.next(), Some(Constant::Str("^_^")));
        assert_eq!(iter.next(), None);
    }
}
