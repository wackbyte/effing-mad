use super::{
    cons::{Cons, Nil},
    index::{Here, There},
    traits::{ToMut, ToRef},
};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Union<Head, Tail> {
    Inl(Head),
    Inr(Tail),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Void {}

pub trait Inject<Element, Index> {
    fn inject(element: Element) -> Self;
}

impl<Head, Tail> Inject<Head, Here> for Union<Head, Tail> {
    fn inject(element: Head) -> Self {
        Self::Inl(element)
    }
}

impl<Head, Tail, Element, Index> Inject<Element, There<Index>> for Union<Head, Tail>
where
    Tail: Inject<Element, Index>,
{
    fn inject(element: Element) -> Self {
        Self::Inr(Tail::inject(element))
    }
}

pub trait Uninject<Element, Index>: Inject<Element, Index> {
    type Remainder;

    fn uninject(self) -> Result<Element, Self::Remainder>;
}

impl<Head, Tail> Uninject<Head, Here> for Union<Head, Tail> {
    type Remainder = Tail;

    fn uninject(self) -> Result<Head, Self::Remainder> {
        match self {
            Self::Inl(head) => Ok(head),
            Self::Inr(tail) => Err(tail),
        }
    }
}

impl<Head, Tail, Element, Index> Uninject<Element, There<Index>> for Union<Head, Tail>
where
    Tail: Uninject<Element, Index>,
{
    type Remainder = Union<Head, Tail::Remainder>;

    fn uninject(self) -> Result<Element, Self::Remainder> {
        match self {
            Self::Inl(head) => Err(Union::Inl(head)),
            Self::Inr(tail) => tail.uninject().map_err(Union::Inr),
        }
    }
}

pub trait Take<Element, Index> {
    fn take(self) -> Option<Element>;
}

impl<Head, Tail> Take<Head, Here> for Union<Head, Tail> {
    fn take(self) -> Option<Head> {
        match self {
            Self::Inl(head) => Some(head),
            _ => None,
        }
    }
}

impl<Head, Tail, Element, Index> Take<Element, There<Index>> for Union<Head, Tail>
where
    Tail: Take<Element, Index>,
{
    fn take(self) -> Option<Element> {
        match self {
            Self::Inr(tail) => tail.take(),
            _ => None,
        }
    }
}

pub trait Get<Element, Index> {
    fn get(&self) -> Option<&Element>;
}

impl<Head, Tail> Get<Head, Here> for Union<Head, Tail> {
    fn get(&self) -> Option<&Head> {
        match self {
            Self::Inl(head) => Some(head),
            _ => None,
        }
    }
}

impl<Head, Tail, Element, Index> Get<Element, There<Index>> for Union<Head, Tail>
where
    Tail: Get<Element, Index>,
{
    fn get(&self) -> Option<&Element> {
        match self {
            Self::Inr(tail) => tail.get(),
            _ => None,
        }
    }
}

pub trait GetMut<Element, Index> {
    fn get_mut(&mut self) -> Option<&mut Element>;
}

impl<Head, Tail> GetMut<Head, Here> for Union<Head, Tail> {
    fn get_mut(&mut self) -> Option<&mut Head> {
        match self {
            Self::Inl(head) => Some(head),
            _ => None,
        }
    }
}

impl<Head, Tail, Element, Index> GetMut<Element, There<Index>> for Union<Head, Tail>
where
    Tail: GetMut<Element, Index>,
{
    fn get_mut(&mut self) -> Option<&mut Element> {
        match self {
            Self::Inr(tail) => tail.get_mut(),
            _ => None,
        }
    }
}

pub trait Subset<Targets, Indices> {
    type Remainder;

    fn subset(self) -> Result<Targets, Self::Remainder>;
}

impl<Choices, Head, Tail, IndicesHead, IndicesTail, Remainder>
    Subset<Union<Head, Tail>, Cons<IndicesHead, IndicesTail>> for Choices
where
    Self: Uninject<Head, IndicesHead, Remainder = Remainder>,
    Remainder: Subset<Tail, IndicesTail>,
{
    type Remainder = <Remainder as Subset<Tail, IndicesTail>>::Remainder;

    fn subset(self) -> Result<Union<Head, Tail>, Self::Remainder> {
        match self.uninject() {
            Ok(good) => Ok(Union::Inl(good)),
            Err(bads) => match bads.subset() {
                Ok(goods) => Ok(Union::Inr(goods)),
                Err(bads) => Err(bads),
            },
        }
    }
}

impl<Choices> Subset<Void, Nil> for Choices {
    type Remainder = Self;

    fn subset(self) -> Result<Void, Self::Remainder> {
        Err(self)
    }
}

pub trait Superset<Targets, Indices> {
    fn superset(self) -> Targets;
}

impl<Head, Tail, IndicesHead, IndicesTail, Targets>
    Superset<Targets, Cons<IndicesHead, IndicesTail>> for Union<Head, Tail>
where
    Tail: Superset<Targets, IndicesTail>,
    Targets: Inject<Head, IndicesHead>,
{
    fn superset(self) -> Targets {
        match self {
            Self::Inl(this) => Targets::inject(this),
            Self::Inr(those) => those.superset(),
        }
    }
}

impl<Head, Tail> Superset<Union<Head, Tail>, Nil> for Void
where
    Void: Superset<Tail, Nil>,
{
    fn superset(self) -> Union<Head, Tail> {
        match self {}
    }
}

impl Superset<Void, Nil> for Void {
    fn superset(self) -> Void {
        self
    }
}

impl<Head, Tail> ToRef for Union<Head, Tail>
where
    Tail: ToRef,
{
    type Output<'a> = Union<&'a Head, <Tail as ToRef>::Output<'a>> where Head: 'a, Tail: 'a;

    fn to_ref<'a>(&'a self) -> Self::Output<'a> {
        match self {
            Self::Inl(head) => Union::Inl(head),
            Self::Inr(tail) => Union::Inr(tail.to_ref()),
        }
    }
}

impl ToRef for Void {
    type Output<'a> = &'a Self;

    fn to_ref<'a>(&'a self) -> Self::Output<'a> {
        self
    }
}

impl<Head, Tail> ToMut for Union<Head, Tail>
where
    Tail: ToMut,
{
    type Output<'a> = Union<&'a mut Head, <Tail as ToMut>::Output<'a>> where Head: 'a, Tail: 'a;

    fn to_mut<'a>(&'a mut self) -> Self::Output<'a> {
        match self {
            Self::Inl(head) => Union::Inl(head),
            Self::Inr(tail) => Union::Inr(tail.to_mut()),
        }
    }
}

impl ToMut for Void {
    type Output<'a> = &'a mut Self;

    fn to_mut<'a>(&'a mut self) -> Self::Output<'a> {
        self
    }
}

macro_rules! impl_common {
    ($T:ident $(<$($G:ident),* $(,)?>)?) => {
        impl $(<$($G),*>)? $T $(<$($G),*>)? {
            pub fn inject<Element, Index>(element: Element) -> Self
            where
                Self: Inject<Element, Index>,
            {
                Inject::inject(element)
            }

            pub fn uninject<Element, Index>(
                self,
            ) -> Result<Element, <Self as Uninject<Element, Index>>::Remainder>
            where
                Self: Uninject<Element, Index>,
            {
                Uninject::uninject(self)
            }

            pub fn take<Element, Index>(self) -> Option<Element>
            where
                Self: Take<Element, Index>,
            {
                Take::take(self)
            }

            pub fn get<Element, Index>(&self) -> Option<&Element>
            where
                Self: Get<Element, Index>,
            {
                Get::get(self)
            }

            pub fn get_mut<Element, Index>(&mut self) -> Option<&mut Element>
            where
                Self: GetMut<Element, Index>,
            {
                GetMut::get_mut(self)
            }

            pub fn subset<Targets, Indices>(
                self,
            ) -> Result<Targets, <Self as Subset<Targets, Indices>>::Remainder>
            where
                Self: Subset<Targets, Indices>,
            {
                Subset::subset(self)
            }

            pub fn superset<Targets, Indices>(self) -> Targets
            where
                Self: Superset<Targets, Indices>,
            {
                Superset::superset(self)
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

impl_common!(Union<Head, Tail>);
impl_common!(Void);

pub macro Union {
    () => {
        $crate::data::union::Void
    },
    ($Head:ty $(,)?) => {
        $crate::data::union::Union<$Head, $crate::data::union::Void>
    },
    ($Head:ty, $($Tail:ty),+ $(,)?) => {
        $crate::data::union::Union<$Head, $crate::data::union::Union!($($Tail),+)>
    },
}
