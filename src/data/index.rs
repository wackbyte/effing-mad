use core::marker::PhantomData;

#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Here {
    marker: (),
}

#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct There<Inner> {
    marker: PhantomData<Inner>,
}
