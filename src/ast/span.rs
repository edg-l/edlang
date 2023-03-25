#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub from: usize,
    pub to: usize,
}

impl Span {
    pub fn new(from: usize, to: usize) -> Self {
        Self { from, to }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SpanValue<T> {
    pub span: Span,
    pub value: T,
}

impl<T> SpanValue<T> {
    pub fn new(l: usize, value: T, r: usize) -> Self {
        Self {
            span: Span::new(l, r),
            value,
        }
    }
}

impl From<Span> for (usize, usize) {
    fn from(value: Span) -> Self {
        (value.from, value.to)
    }
}
