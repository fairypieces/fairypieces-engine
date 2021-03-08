use std::hash::Hash;
use std::ops::{Deref, DerefMut, Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Rem, RemAssign, Sub, SubAssign};
use std::fmt::{Display, Debug};
use generic_array::typenum;
use generic_array::{GenericArray, ArrayLength};

pub trait IVecLength = ArrayLength<i32, ArrayType: Copy> + Hash + PartialEq + Eq + PartialOrd + Ord;

#[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct IVec<D: IVecLength>(pub GenericArray<i32, D>);

impl<D: IVecLength> Display for IVec<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<'a, D: IVecLength> IntoIterator for &'a IVec<D> {
    type Item = &'a i32;
    type IntoIter = <&'a GenericArray<i32, D> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a, D: IVecLength> IntoIterator for &'a mut IVec<D> {
    type Item = &'a mut i32;
    type IntoIter = <&'a mut GenericArray<i32, D> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

impl<D: IVecLength> IntoIterator for IVec<D> {
    type Item = i32;
    type IntoIter = <GenericArray<i32, D> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<D: IVecLength> AddAssign<i32> for IVec<D> {
    fn add_assign(&mut self, rhs: i32) {
        self.iter_mut().for_each(|c| c.add_assign(rhs));
    }
}

impl<D: IVecLength> Add<i32> for IVec<D> {
    type Output = Self;

    fn add(mut self, rhs: i32) -> Self::Output {
        self.add_assign(rhs);
        self
    }
}

impl<D: IVecLength> AddAssign<Self> for IVec<D> {
    fn add_assign(&mut self, rhs: Self) {
        self.iter_mut()
            .zip(rhs.into_iter())
            .for_each(|(c, rhs_c)| c.add_assign(rhs_c));
    }
}

impl<D: IVecLength> Add<Self> for IVec<D> {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        self.add_assign(rhs);
        self
    }
}

impl<D: IVecLength> SubAssign<i32> for IVec<D> {
    fn sub_assign(&mut self, rhs: i32) {
        self.iter_mut().for_each(|c| c.sub_assign(rhs));
    }
}

impl<D: IVecLength> Sub<i32> for IVec<D> {
    type Output = Self;

    fn sub(mut self, rhs: i32) -> Self::Output {
        self.sub_assign(rhs);
        self
    }
}

impl<D: IVecLength> SubAssign<Self> for IVec<D> {
    fn sub_assign(&mut self, rhs: Self) {
        self.iter_mut()
            .zip(rhs.into_iter())
            .for_each(|(c, rhs_c)| c.sub_assign(rhs_c));
    }
}

impl<D: IVecLength> Sub<Self> for IVec<D> {
    type Output = Self;

    fn sub(mut self, rhs: Self) -> Self::Output {
        self.sub_assign(rhs);
        self
    }
}

impl<D: IVecLength> MulAssign<i32> for IVec<D> {
    fn mul_assign(&mut self, rhs: i32) {
        self.iter_mut().for_each(|c| c.mul_assign(rhs));
    }
}

impl<D: IVecLength> Mul<i32> for IVec<D> {
    type Output = Self;

    fn mul(mut self, rhs: i32) -> Self::Output {
        self.mul_assign(rhs);
        self
    }
}

impl<D: IVecLength> MulAssign<Self> for IVec<D> {
    fn mul_assign(&mut self, rhs: Self) {
        self.iter_mut()
            .zip(rhs.into_iter())
            .for_each(|(c, rhs_c)| c.mul_assign(rhs_c));
    }
}

impl<D: IVecLength> Mul<Self> for IVec<D> {
    type Output = Self;

    fn mul(mut self, rhs: Self) -> Self::Output {
        self.mul_assign(rhs);
        self
    }
}

impl<D: IVecLength> DivAssign<i32> for IVec<D> {
    fn div_assign(&mut self, rhs: i32) {
        self.iter_mut().for_each(|c| c.div_assign(rhs));
    }
}

impl<D: IVecLength> Div<i32> for IVec<D> {
    type Output = Self;

    fn div(mut self, rhs: i32) -> Self::Output {
        self.div_assign(rhs);
        self
    }
}

impl<D: IVecLength> DivAssign<Self> for IVec<D> {
    fn div_assign(&mut self, rhs: Self) {
        self.iter_mut()
            .zip(rhs.into_iter())
            .for_each(|(c, rhs_c)| c.div_assign(rhs_c));
    }
}

impl<D: IVecLength> Div<Self> for IVec<D> {
    type Output = Self;

    fn div(mut self, rhs: Self) -> Self::Output {
        self.div_assign(rhs);
        self
    }
}

impl<D: IVecLength> RemAssign<i32> for IVec<D> {
    fn rem_assign(&mut self, rhs: i32) {
        self.iter_mut().for_each(|c| c.rem_assign(rhs));
    }
}

impl<D: IVecLength> Rem<i32> for IVec<D> {
    type Output = Self;

    fn rem(mut self, rhs: i32) -> Self::Output {
        self.rem_assign(rhs);
        self
    }
}

impl<D: IVecLength> RemAssign<Self> for IVec<D> {
    fn rem_assign(&mut self, rhs: Self) {
        self.iter_mut()
            .zip(rhs.into_iter())
            .for_each(|(c, rhs_c)| c.rem_assign(rhs_c));
    }
}

impl<D: IVecLength> Rem<Self> for IVec<D> {
    type Output = Self;

    fn rem(mut self, rhs: Self) -> Self::Output {
        self.rem_assign(rhs);
        self
    }
}

impl<D: IVecLength> Neg for IVec<D> {
    type Output = Self;

    fn neg(mut self) -> Self::Output {
        self.iter_mut().for_each(|c| *c = c.neg());
        self
    }
}

impl<D: IVecLength> Deref for IVec<D> {
    type Target = [i32];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<D: IVecLength> DerefMut for IVec<D> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<D: IVecLength, T: Into<GenericArray<i32, D>>> From<T> for IVec<D> {
    fn from(from: T) -> Self {
        Self(from.into())
    }
}

pub type IVec0 = IVec<typenum::U0>;
pub type IVec1 = IVec<typenum::U1>;
pub type IVec2 = IVec<typenum::U2>;
pub type IVec3 = IVec<typenum::U3>;
pub type IVec4 = IVec<typenum::U4>;
