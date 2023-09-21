#[derive(Clone, Debug)]
pub struct Cursor<'a, T> {
    data: &'a [T],
    offset: usize,
}

impl<'a, T> PartialEq for Cursor<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        self.data.as_ptr() == other.data.as_ptr()
            && self.data.len() == other.data.len()
            && self.offset == other.offset
    }
}

impl<'a, T: 'a> std::ops::Index<usize> for Cursor<'a, T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.data[self.offset + index]
    }
}

macro_rules! impl_range {
    ( $range:ty ) => {
        impl<'a, T> std::ops::Index<$range> for Cursor<'a, T> {
            type Output = [T];

            fn index(&self, range: $range) -> &Self::Output {
                &self.data[self.offset..][range]
            }
        }
    };
}

impl_range!(std::ops::Range<usize>);
impl_range!(std::ops::RangeFrom<usize>);
impl_range!(std::ops::RangeFull);
impl_range!(std::ops::RangeInclusive<usize>);
impl_range!(std::ops::RangeTo<usize>);
impl_range!(std::ops::RangeToInclusive<usize>);

impl<'a, T> From<Cursor<'a, T>> for &'a [T] {
    fn from(cursor: Cursor<'a, T>) -> Self {
        &cursor.data[cursor.offset..]
    }
}

impl<'a, T> From<&'_ Cursor<'a, T>> for &'a [T] {
    fn from(cursor: &'_ Cursor<'a, T>) -> Self {
        &cursor.data[cursor.offset..]
    }
}

impl<'a, T> From<&'a [T]> for Cursor<'a, T> {
    fn from(data: &'a [T]) -> Self {
        Self { data, offset: 0 }
    }
}

impl<'a, T, const N: usize> From<&'a [T; N]> for Cursor<'a, T> {
    fn from(data: &'a [T; N]) -> Self {
        Self { data, offset: 0 }
    }
}

impl<'a, T> std::ops::Deref for Cursor<'a, T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.data[self.offset..]
    }
}

impl<'a, T> Cursor<'a, T> {
    pub fn new(data: &'a [T], offset: usize) -> Self {
        Self { data, offset }
    }

    pub fn take(&mut self, amount: usize) -> &'a [T] {
        let head = self.peek(amount);
        self.offset += amount;
        head
    }

    pub fn peek(&self, amount: usize) -> &'a [T] {
        if self.offset >= self.data.len() {
            return &[];
        }
        let max = (self.offset + amount).min(self.data.len());

        &self.data[self.offset..max]
    }

    pub fn get(&self, at: usize) -> Option<&'a T> {
        self.data.get(self.offset + at)
    }

    pub fn iter<'b>(&'b self) -> impl Iterator<Item = &'a T> + 'b {
        self.data[self.offset..].iter()
    }

    pub fn len(&self) -> usize {
        self.data.len().saturating_sub(self.offset)
    }

    pub fn range<R: std::slice::SliceIndex<[T], Output = [T]>>(&self, range: R) -> Self {
        Self {
            data: &self.data[self.offset..][range],
            offset: 0,
        }
    }
}
