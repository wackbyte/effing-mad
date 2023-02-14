pub trait ToRef {
    type Output<'a>
    where
        Self: 'a;

    fn to_ref<'a>(&'a self) -> Self::Output<'a>;
}

pub trait ToMut {
    type Output<'a>
    where
        Self: 'a;

    fn to_mut<'a>(&'a mut self) -> Self::Output<'a>;
}
