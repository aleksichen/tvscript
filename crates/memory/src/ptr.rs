use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    ops::Deref,
    rc::Rc,
};

use crate::address::Address;

/// 创建支持trait对象转换的智能指针的宏
///
/// 虽然可以直接使用Ptr::new，但当需要将具体类型转换为`dyn Trait`时，
/// 在`CoerceUnized`特性稳定之前，需要通过内部指针进行转换。
/// 本宏封装了类型转换逻辑，简化调用方代码。
#[macro_export]
macro_rules! make_ptr {
    ($value:expr) => {{
        use std::rc::Rc;

        Ptr::from(Rc::new($value) as Rc<_>)
    }};
}

/// 不可变智能指针，用于管理堆内存中的值
#[derive(Debug, Default)]
pub struct Ptr<T: ?Sized>(Rc<T>);  // 内部使用Rc实现引用计数

/// 从具体值创建Ptr
impl<T> From<T> for Ptr<T> {
    fn from(value: T) -> Self {
        Self(value.into())
    }
}

/// 从Box转换，保留原有分配
impl<T: ?Sized> From<Box<T>> for Ptr<T> {
    fn from(boxed: Box<T>) -> Self {
        Self(boxed.into())
    }
}

/// 从Rc直接转换
impl<T: ?Sized> From<Rc<T>> for Ptr<T> {
    fn from(inner: Rc<T>) -> Self {
        Self(inner)
    }
}

/// 核心方法实现
impl<T: ?Sized> Ptr<T> {
    /// 检查两个指针是否指向同一内存地址
    ///
    /// 另见: [std::rc::Rc::ptr_eq]
    pub fn ptr_eq(this: &Self, other: &Self) -> bool {
        Rc::ptr_eq(&this.0, &other.0)
    }

    /// 获取分配内存的地址
    pub fn address(this: &Self) -> Address {
        Rc::as_ptr(&this.0).into()
    }

    /// 获取当前强引用计数
    ///
    /// 注意：仅统计强引用，弱引用不计入结果
    pub fn ref_count(this: &Self) -> usize {
        Rc::strong_count(&this.0)
    }
}

/// 可变访问支持 (需要T实现Clone)
impl<T: Clone> Ptr<T> {
    /// 获取可变引用，必要时克隆数据
    ///
    /// 如果当前是唯一所有者，直接返回可变引用
    /// 否则克隆数据确保唯一性后返回新引用的可变引用
    ///
    /// 另见: [std::rc::Rc::make_mut]
    pub fn make_mut(this: &mut Self) -> &mut T {
        Rc::make_mut(&mut this.0)
    }
}

/// 解引用实现
impl<T: ?Sized> Deref for Ptr<T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.0.deref()
    }
}

/// 克隆实现（增加引用计数）
impl<T: ?Sized> Clone for Ptr<T> {
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }
}

// 以下为各种类型的转换实现 -----------------------------

/// 从切片创建Ptr<[T]>
impl<T: Clone> From<&[T]> for Ptr<[T]> {
    #[inline]
    fn from(value: &[T]) -> Self {
        Self(Rc::from(value))
    }
}

/// 从Vec创建Ptr<[T]>
impl<T> From<Vec<T>> for Ptr<[T]> {
    #[inline]
    fn from(value: Vec<T>) -> Self {
        Self(Rc::from(value))
    }
}

/// 从字符串切片创建Ptr<str>
impl From<&str> for Ptr<str> {
    #[inline]
    fn from(value: &str) -> Self {
        Self(Rc::from(value))
    }
}

/// 从String创建Ptr<str>
impl From<String> for Ptr<str> {
    #[inline]
    fn from(value: String) -> Self {
        Self(Rc::from(value))
    }
}

// 特性实现 -----------------------------

/// 相等性比较（基于内容相等）
impl<T: ?Sized + PartialEq> PartialEq for Ptr<T> {
    fn eq(&self, other: &Self) -> bool {
        Rc::eq(&self.0, &other.0)
    }
}

/// 等式特性标记
impl<T: ?Sized + Eq> Eq for Ptr<T> {}

/// 格式化输出代理
impl<T: ?Sized + fmt::Display> fmt::Display for Ptr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// 哈希实现代理
impl<T: ?Sized + Hash> Hash for Ptr<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

/// 全序比较实现
impl<T: ?Sized + Ord> Ord for Ptr<T> {
    #[inline]
    fn cmp(&self, other: &Ptr<T>) -> Ordering {
        self.0.cmp(&other.0)
    }
}

/// 部分序比较实现
impl<T: ?Sized + PartialOrd> PartialOrd for Ptr<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // 测试trait对象支持
    trait DummyTrait: fmt::Debug {
        fn value(&self) -> i32;
    }

    #[derive(Debug)]
    struct DummyStruct(i32);
    
    impl DummyTrait for DummyStruct {
        fn value(&self) -> i32 {
            self.0
        }
    }

    #[test]
    fn basic_functionality() {
        // 测试基础创建和引用计数
        let ptr = Ptr::from(42);
        assert_eq!(*ptr, 42);
        assert_eq!(Ptr::ref_count(&ptr), 1);

        let cloned = ptr.clone();
        assert_eq!(Ptr::ref_count(&ptr), 2);
        assert!(Ptr::ptr_eq(&ptr, &cloned));
    }

    #[test]
    fn test_make_ptr() {
        // 测试宏创建和trait对象转换
        let ptr = make_ptr!(DummyStruct(100));
        let trait_ptr: Ptr<dyn DummyTrait> = ptr;
        assert_eq!(trait_ptr.value(), 100);
    }

    #[test]
    fn conversions() {
        // 测试各种From实现
        let from_box: Ptr<i32> = Ptr::from(Box::new(42));
        assert_eq!(*from_box, 42);

        let vec_ptr: Ptr<[i32]> = Ptr::from(vec![1, 2, 3]);
        assert_eq!(&*vec_ptr, &[1, 2, 3]);

        let str_ptr: Ptr<str> = Ptr::from("test");
        assert_eq!(&*str_ptr, "test");
    }

    #[test]
    fn equality() {
        // 测试相等性比较
        let ptr1 = Ptr::from(101);
        let ptr2 = Ptr::from(100);
        let ptr3 = ptr1.clone();

        assert_ne!(ptr1, ptr2); // 不同分配
        assert_eq!(ptr1, ptr3); // 同一分配
    }

    #[test]
    fn ordering() {
        // 测试排序比较
        let ptr_a = Ptr::from(10);
        let ptr_b = Ptr::from(20);
        assert!(ptr_a < ptr_b);
    }

    #[test]
    fn make_mut_behavior() {
        // 测试可变修改行为
        let mut ptr = Ptr::from(42);
        assert_eq!(Ptr::ref_count(&ptr), 1);

        // 唯一引用直接修改
        *Ptr::make_mut(&mut ptr) = 100;
        assert_eq!(*ptr, 100);

        let cloned = ptr.clone();
        assert_eq!(Ptr::ref_count(&ptr), 2);

        // 非唯一引用时克隆
        *Ptr::make_mut(&mut ptr) = 200;
        assert_eq!(*ptr, 200);
        assert_eq!(*cloned, 100); // 原数据保持不变
    }

    #[test]
    fn address_retrieval() {
        // 测试地址获取
        let ptr = Ptr::from(42);
        let addr = Ptr::address(&ptr);
        assert_ne!(addr, Address(std::ptr::null()));
    }

    #[test]
    fn hash_implementation() {
        // 测试哈希一致性
        use std::collections::hash_map::DefaultHasher;

        let ptr: Ptr<str> = Ptr::from("test");
        let mut hasher1 = DefaultHasher::new();
        ptr.hash(&mut hasher1);

        let cloned = ptr.clone();
        let mut hasher2 = DefaultHasher::new();
        cloned.hash(&mut hasher2);

        assert_eq!(hasher1.finish(), hasher2.finish());
    }

    #[test]
    fn display_implementation() {
        // 测试格式化输出
        let ptr = Ptr::from(3.14);
        assert_eq!(format!("{}", ptr), "3.14");
    }
}
