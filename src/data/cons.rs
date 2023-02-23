use super::{
    index::{Here, There},
    traits::{ToMut, ToRef},
};

#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Nil;

#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Cons<Head, Tail> {
    pub head: Head,
    pub tail: Tail,
}

pub trait List {
    const LEN: usize;

    fn len(&self) -> usize {
        Self::LEN
    }

    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl List for Nil {
    const LEN: usize = 0;
}

impl<Head, Tail> List for Cons<Head, Tail>
where
    Tail: List,
{
    const LEN: usize = 1 + <Tail as List>::LEN;
}

pub trait Append<Other> {
    type Output;

    fn append(self, other: Other) -> Self::Output;
}

impl Append<Nil> for Nil {
    type Output = Self;

    fn append(self, _: Nil) -> Self::Output {
        self
    }
}

impl<Head, Tail> Append<Nil> for Cons<Head, Tail> {
    type Output = Self;

    fn append(self, _: Nil) -> Self::Output {
        self
    }
}

impl<Head, Tail> Append<Cons<Head, Tail>> for Nil {
    type Output = Cons<Head, Tail>;

    fn append(self, other: Cons<Head, Tail>) -> Self::Output {
        other
    }
}

impl<LeftHead, LeftTail, RightHead, RightTail> Append<Cons<RightHead, RightTail>>
    for Cons<LeftHead, LeftTail>
where
    LeftTail: Append<Cons<RightHead, RightTail>>,
{
    type Output = Cons<LeftHead, <LeftTail as Append<Cons<RightHead, RightTail>>>::Output>;

    fn append(self, other: Cons<RightHead, RightTail>) -> Self::Output {
        Cons {
            head: self.head,
            tail: self.tail.append(other),
        }
    }
}

pub trait Prepend<Other> {
    type Output;

    fn prepend(self, other: Other) -> Self::Output;
}

impl<Left, Right> Prepend<Right> for Left
where
    Right: Append<Left>,
{
    type Output = <Right as Append<Left>>::Output;

    fn prepend(self, other: Right) -> Self::Output {
        other.append(self)
    }
}

pub trait Insert<Other, Index> {
    type Output;

    fn insert(self, other: Other) -> Self::Output;
}

impl<Head, Tail, Other> Insert<Other, Here> for Cons<Head, Tail>
where
    Other: Append<Tail>,
{
    type Output = Cons<Head, <Other as Append<Tail>>::Output>;

    fn insert(self, other: Other) -> Self::Output {
        Cons {
            head: self.head,
            tail: other.append(self.tail),
        }
    }
}

pub trait Take<Element, Index> {
    fn take(self) -> Element;
}

impl<Head, Tail> Take<Head, Here> for Cons<Head, Tail> {
    fn take(self) -> Head {
        self.head
    }
}

impl<Head, Tail, Element, Index> Take<Element, There<Index>> for Cons<Head, Tail>
where
    Tail: Take<Element, Index>,
{
    fn take(self) -> Element {
        self.tail.take()
    }
}

pub trait Get<Element, Index> {
    fn get(&self) -> &Element;
}

impl<Head, Tail> Get<Head, Here> for Cons<Head, Tail> {
    fn get(&self) -> &Head {
        &self.head
    }
}

impl<Head, Tail, Element, Index> Get<Element, There<Index>> for Cons<Head, Tail>
where
    Tail: Get<Element, Index>,
{
    fn get(&self) -> &Element {
        self.tail.get()
    }
}

pub trait GetMut<Element, Index> {
    fn get_mut(&mut self) -> &mut Element;
}

impl<Head, Tail> GetMut<Head, Here> for Cons<Head, Tail> {
    fn get_mut(&mut self) -> &mut Head {
        &mut self.head
    }
}

impl<Head, Tail, Element, Index> GetMut<Element, There<Index>> for Cons<Head, Tail>
where
    Tail: GetMut<Element, Index>,
{
    fn get_mut(&mut self) -> &mut Element {
        self.tail.get_mut()
    }
}

pub trait Pluck<Target, Index> {
    type Remainder;

    fn pluck(self) -> (Target, Self::Remainder);
}

impl<Head, Tail> Pluck<Head, Here> for Cons<Head, Tail> {
    type Remainder = Tail;

    fn pluck(self) -> (Head, Self::Remainder) {
        (self.head, self.tail)
    }
}

impl<Head, Tail, Target, Index> Pluck<Target, There<Index>> for Cons<Head, Tail>
where
    Tail: Pluck<Target, Index>,
{
    type Remainder = Cons<Head, <Tail as Pluck<Target, Index>>::Remainder>;

    fn pluck(self) -> (Target, Self::Remainder) {
        let (target, tail_remainder): (Target, <Tail as Pluck<Target, Index>>::Remainder) =
            <Tail as Pluck<Target, Index>>::pluck(self.tail);
        (
            target,
            Cons {
                head: self.head,
                tail: tail_remainder,
            },
        )
    }
}

pub trait Sculpt<Target, Indices> {
    type Remainder;

    fn sculpt(self) -> (Target, Self::Remainder);
}

impl<Source> Sculpt<Nil, Nil> for Source {
    type Remainder = Source;

    fn sculpt(self) -> (Nil, Self::Remainder) {
        (Nil, self)
    }
}

impl<SourceHead, SourceTail, TargetHead, TargetTail, IndicesHead, IndicesTail>
    Sculpt<Cons<TargetHead, TargetTail>, Cons<IndicesHead, IndicesTail>>
    for Cons<SourceHead, SourceTail>
where
    Cons<SourceHead, SourceTail>: Pluck<TargetHead, IndicesHead>,
    <Cons<SourceHead, SourceTail> as Pluck<TargetHead, IndicesHead>>::Remainder:
        Sculpt<TargetTail, IndicesTail>,
{
    type Remainder =
        <<Cons<SourceHead, SourceTail> as Pluck<TargetHead, IndicesHead>>::Remainder as Sculpt<
            TargetTail,
            IndicesTail,
        >>::Remainder;

    fn sculpt(self) -> (Cons<TargetHead, TargetTail>, Self::Remainder) {
        let (target_head, remainder): (
            TargetHead,
            <Cons<SourceHead, SourceTail> as Pluck<TargetHead, IndicesHead>>::Remainder,
        ) = self.pluck();
        let (target_tail, remainder): (TargetTail, Self::Remainder) = remainder.sculpt();
        (
            Cons {
                head: target_head,
                tail: target_tail,
            },
            remainder,
        )
    }
}

impl ToRef for Nil {
    type Output<'a> = Self;

    fn to_ref<'a>(&'a self) -> Self::Output<'a> {
        *self
    }
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

impl ToMut for Nil {
    type Output<'a> = Self;

    fn to_mut<'a>(&'a mut self) -> Self::Output<'a> {
        *self
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

macro_rules! impl_common {
    ($T:ident $(<$($G:ident),* $(,)?>)?) => {
        impl $(<$($G),*>)? $T $(<$($G),*>)? {
            pub fn len(&self) -> usize where Self: List {
                List::len(self)
            }

            pub fn is_empty(&self) -> bool where Self: List {
                List::is_empty(self)
            }

            pub fn take<Element, Index>(self) -> Element where Self: Take<Element, Index> {
                Take::take(self)
            }

            pub fn get<Element, Index>(&self) -> &Element where Self: Get<Element, Index> {
                Get::get(self)
            }

            pub fn get_mut<Element, Index>(&mut self) -> &mut Element where Self: GetMut<Element, Index> {
                GetMut::get_mut(self)
            }

            pub fn append<Other>(self, other: Other) -> <Self as Append<Other>>::Output where Self: Append<Other> {
                Append::append(self, other)
            }

            pub fn prepend<Other>(self, other: Other) -> <Self as Prepend<Other>>::Output where Self: Prepend<Other> {
                Prepend::prepend(self, other)
            }

            pub fn pluck<Target, Index>(self) -> (Target, <Self as Pluck<Target, Index>>::Remainder) where Self: Pluck<Target, Index> {
                Pluck::pluck(self)
            }

            pub fn sculpt<Target, Indices>(self) -> (Target, <Self as Sculpt<Target, Indices>>::Remainder) where Self: Sculpt<Target, Indices> {
                Sculpt::sculpt(self)
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
        $crate::cons::Nil
    },
    ($Head:ty $(,)?) => {
        $crate::cons::Cons<$Head, $crate::cons::Nil>
    },
    ($Head:ty, $($Tail:ty),* $(,)?) => {
        $crate::cons::Cons<$Head, $crate::cons::Cons!($($Tail),*)>
    },
}

pub macro cons {
    () => {
        $crate::cons::Nil
    },
    ($head:expr $(,)?) => {
        $crate::cons::Cons {
            head: $head,
            tail: $crate::cons::Nil,
        }
    },
    ($head:expr, $($tail:expr),+ $(,)?) => {
        $crate::cons::Cons {
            head: $head,
            tail: $crate::cons::cons!($($tail),+),
        }
    },
}
