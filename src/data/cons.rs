use super::{
    index::{Here, There},
    traits::{ToMut, ToRef},
};

#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Cons<Head, Tail> {
    pub head: Head,
    pub tail: Tail,
}

#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Nil;

pub trait List {
    const LEN: usize;

    fn len(&self) -> usize {
        Self::LEN
    }

    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<Head, Tail> List for Cons<Head, Tail>
where
    Tail: List,
{
    const LEN: usize = 1 + <Tail as List>::LEN;
}

impl List for Nil {
    const LEN: usize = 0;
}

impl<Head, Tail> ToRef for Cons<Head, Tail>
where
    Tail: ToRef,
{
    type Output<'a> = Cons<&'a Head, <Tail as ToRef>::Output<'a>> where Head: 'a, Tail: 'a;

    fn to_ref<'a>(&'a self) -> Self::Output<'a> {
        Cons {
            head: &self.head,
            tail: self.tail.to_ref(),
        }
    }
}

impl ToRef for Nil {
    type Output<'a> = &'a Self;

    fn to_ref<'a>(&'a self) -> Self::Output<'a> {
        self
    }
}

impl<Head, Tail> ToMut for Cons<Head, Tail>
where
    Tail: ToMut,
{
    type Output<'a> = Cons<&'a mut Head, <Tail as ToMut>::Output<'a>> where Head: 'a, Tail: 'a;

    fn to_mut<'a>(&'a mut self) -> Self::Output<'a> {
        Cons {
            head: &mut self.head,
            tail: self.tail.to_mut(),
        }
    }
}

impl ToMut for Nil {
    type Output<'a> = &'a mut Self;

    fn to_mut<'a>(&'a mut self) -> Self::Output<'a> {
        self
    }
}

macro_rules! impl_common {
    ($T:ident $(<$($G:ident),* $(,)?>)?) => {
        impl $(<$($G),*>)? $T $(<$($G),*>)? {
            pub fn len(&self) -> usize where Self: List {
                List::len(self)
            }

            pub fn is_empty(&self) -> bool where Self: List {
                List::is_empty(self)
            }

            pub fn to_ref<'a>(&'a self) -> <Self as ToRef>::Output<'a>
            where
                Self: ToRef,
            {
                ToRef::to_ref(self)
            }

            pub fn to_mut<'a>(&'a mut self) -> <Self as ToMut>::Output<'a>
            where
                Self: ToMut,
            {
                ToMut::to_mut(self)
            }
        }
    };
}

impl_common!(Cons<Head, Tail>);
impl_common!(Nil);

pub macro Cons {
    () => {
        $crate::data::list::Nil
    },
    ($Head:ty $(,)?) => {
        $crate::data::list::Cons<$Head, $crate::data::list::Nil>,
    },
    ($Head:ty, $($Tail:ty),* $(,)?) => {
        $crate::data::list::Cons<$Head, $crate::data::list::Cons!($($Tail),*)>
    },
}

pub macro cons {
    () => {
        $crate::data::list::Nil
    },
    ($head:expr $(,)?) => {
        $crate::data::list::Cons {
            head: $head,
            tail: $crate::data::list::Nil,
        }
    },
    ($head:expr, $($tail:expr),+ $(,)?) => {
        $crate::data::list::Cons {
            head: $head,
            tail: $crate::data::list::cons!($($tail),+),
        }
    },
}
