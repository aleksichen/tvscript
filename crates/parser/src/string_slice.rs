use std::ops::{Deref, Range};

use memory::ptr::Ptr;

pub trait StringSliceIndex: TryFrom<usize> + Copy + Default {
    fn to_usize(self) -> usize;
}

impl StringSliceIndex for usize {
    fn to_usize(self) -> usize {
        self
    }
}

impl StringSliceIndex for u16 {
    fn to_usize(self) -> usize {
        self as usize
    }
}

#[derive(Clone, Debug)]
pub struct StringSlice<T> {
    data: Ptr<String>,
    bounds: Range<T>,
    _niche: bool,
}

impl<T> StringSlice<T>
where
    T: StringSliceIndex,
{
    pub fn new(string: Ptr<String>, bounds: Range<usize>) -> Option<Self> {
        try_from_range(&bounds).map(|bounds| Self {
            data: string,
            bounds,
            _niche: false,
        })
    }

    pub unsafe fn new_unchecked(string: Ptr<String>, bounds: Range<T>) -> Self {
        Self {
            data: string,
            bounds,
            _niche: false,
        }
    }

    pub fn with_bounds(&self, bounds: Range<usize>) -> Option<Self> {
        let new_bounds = (bounds.start + self.bounds.start.to_usize())
            ..(bounds.end + self.bounds.start.to_usize());

        if self.data.get(new_bounds.clone()).is_some() {
            try_from_range(&new_bounds).map(|bounds| Self {
                data: self.data.clone(),
                bounds,
                _niche: false,
            })
        } else {
            None
        }
    }

    pub fn try_convert<U>(&self) -> Option<StringSlice<U>>
    where
        U: TryFrom<T>,
    {
        try_from_range(&self.bounds).map(|bounds| StringSlice::<U> {
            data: self.data.clone(),
            bounds,
            _niche: false,
        })
    }

    pub fn as_str(&self) -> &str {
        unsafe { self.data.get_unchecked(to_usize_range(&self.bounds)) }
    }

    pub fn split(&self, offset: usize) -> Option<(Self, Self)> {
        let split_point = self.bounds.start.to_usize() + offset;
        if self.data.is_char_boundary(split_point) {
            if let Ok(split_point_t) = T::try_from(split_point) {
                Some((
                    Self {
                        data: self.data.clone(),
                        bounds: self.bounds.start..split_point_t,
                        _niche: false,
                    },
                    Self {
                        data: self.data.clone(),
                        bounds: split_point_t..self.bounds.end,
                        _niche: false,
                    },
                ))
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl From<Ptr<String>> for StringSlice<usize> {
    fn from(string: Ptr<String>) -> Self {
        let bounds = 0..string.len();
        Self {
            data: string,
            bounds,
            _niche: false,
        }
    }
}

impl From<String> for StringSlice<usize> {
    fn from(string: String) -> Self {
        let bounds = 0..string.len();
        Self {
            data: string.into(),
            bounds,
            _niche: false,
        }
    }
}

impl From<&str> for StringSlice<usize> {
    fn from(string: &str) -> Self {
        Self::from(string.to_string())
    }
}

impl<T> Deref for StringSlice<T>
where
    T: StringSliceIndex,
{
    type Target = str;

    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl<T> AsRef<str> for StringSlice<T>
where
    T: StringSliceIndex,
{
    fn as_ref(&self) -> &str {
        self.deref()
    }
}

impl<T> PartialEq<StringSlice<T>> for StringSlice<T>
where
    T: StringSliceIndex,
{
    fn eq(&self, other: &StringSlice<T>) -> bool {
        self.as_str() == other.as_str()
    }
}

impl<T> PartialEq<&str> for StringSlice<T>
where
    T: StringSliceIndex,
{
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

fn to_usize_range<T>(r: &Range<T>) -> Range<usize>
where
    T: StringSliceIndex,
{
    r.start.to_usize()..r.end.to_usize()
}

fn try_from_range<T, U>(r: &Range<U>) -> Option<Range<T>>
where
    T: TryFrom<U>,
    U: Copy,
{
    match (T::try_from(r.start), T::try_from(r.end)) {
        (Ok(start), Ok(end)) => Some(start..end),
        _ => None,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn new() {
        let string = Ptr::from("abcdef".to_string());
        let slice = StringSlice::<usize>::new(string, 1..3).unwrap();
        assert_eq!(slice.as_str(), "bc");
    }

    #[test]
    fn with_bounds() {
        let original = StringSlice::from("0123456789");
        let slice = original.with_bounds(4..8).unwrap();
        assert_eq!(slice.as_str(), "4567");
    }

    #[test]
    fn split() {
        let original = StringSlice::from("hello, world!");
        let (a, b) = original.split(6).unwrap();
        assert_eq!(a.as_str(), "hello,");
        assert_eq!(b.as_str(), " world!");
    }

    #[test]
    fn equality() {
        let s1 = StringSlice::from("abc".to_string());
        let s2 = StringSlice::from("xyz");
        let s3 = StringSlice::from("___xyz___").with_bounds(3..6).unwrap();
        assert_ne!(s1, s2);
        assert_ne!(s1, s3);
        assert_eq!(s2, s3);
        assert_eq!(s2, "xyz");
        assert_eq!(s3, "xyz");
    }
}
