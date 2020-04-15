module Data.Generic.Rep.Derive where

import Prelude

import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (class GenericBottom, class GenericTop, genericBottom, genericTop)
import Data.Generic.Rep.Enum (class GenericBoundedEnum, class GenericEnum, genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Eq (class GenericEq, genericEq)
import Data.Generic.Rep.HeytingAlgebra (class GenericHeytingAlgebra, genericConj, genericDisj, genericFF, genericImplies, genericNot, genericTT)
import Data.Generic.Rep.Monoid (class GenericMonoid, genericMempty)
import Data.Generic.Rep.Ord (class GenericOrd, genericCompare)
import Data.Generic.Rep.Ring (class GenericRing, genericSub)
import Data.Generic.Rep.Semigroup (class GenericSemigroup, genericAppend)
import Data.Generic.Rep.Semiring (class GenericSemiring, genericAdd, genericMul, genericOne, genericZero)
import Data.Generic.Rep.Show (class GenericShow, genericShow)
import Data.Newtype (class Newtype, over, over2, traverse, unwrap)

newtype UsingGeneric a = UsingGeneric a

derive instance newtypeUsingGeneric :: Newtype (UsingGeneric a) _

instance boundedUsingGeneric ::
  ( Generic a rep
  , GenericBottom rep
  , GenericEq rep
  , GenericOrd rep
  , GenericTop rep
  ) => Bounded (UsingGeneric a) where
  top = UsingGeneric genericTop
  bottom = UsingGeneric genericBottom

instance boundedEnumUsingGeneric ::
  ( Generic a rep
  , GenericBottom rep
  , GenericBoundedEnum rep
  , GenericEnum rep
  , GenericEq rep
  , GenericOrd rep
  , GenericTop rep
  ) => BoundedEnum (UsingGeneric a) where
  cardinality = Cardinality $ unwrap (genericCardinality :: Cardinality a)
  toEnum = genericToEnum >>> map UsingGeneric
  fromEnum = unwrap >>> genericFromEnum

instance enumUsingGeneric ::
  ( Generic a rep
  , GenericEnum rep
  , GenericEq rep
  , GenericOrd rep
  ) => Enum (UsingGeneric a) where
  succ = traverse UsingGeneric genericSucc
  pred = traverse UsingGeneric genericPred

instance eqUsingGeneric ::
  ( Generic a rep
  , GenericEq rep
  ) => Eq (UsingGeneric a) where
  eq = genericEq `on` unwrap

instance heytingAlgebraUsingGeneric ::
  ( Generic a rep
  , GenericHeytingAlgebra rep
  ) => HeytingAlgebra (UsingGeneric a) where
  ff = UsingGeneric genericFF
  tt = UsingGeneric genericTT
  implies = over2 UsingGeneric genericImplies
  conj = over2 UsingGeneric genericConj
  disj = over2 UsingGeneric genericDisj
  not = over UsingGeneric genericNot

instance monoidUsingGeneric ::
  ( Generic a rep
  , GenericMonoid rep
  , GenericSemigroup rep
  ) => Monoid (UsingGeneric a) where
  mempty = UsingGeneric genericMempty

instance ordUsingGeneric ::
  ( Generic a rep
  , GenericEq rep
  , GenericOrd rep
  ) => Ord (UsingGeneric a) where
  compare = genericCompare `on` unwrap

instance ringUsingGeneric ::
  ( Generic a rep
  , GenericRing rep
  , GenericSemiring rep
  ) => Ring (UsingGeneric a) where
  sub = over2 UsingGeneric genericSub

instance semigroupUsingGeneric ::
  ( Generic a rep
  , GenericSemigroup rep
  ) => Semigroup (UsingGeneric a) where
  append = over2 UsingGeneric genericAppend

instance semiringUsingGeneric ::
  ( Generic a rep
  , GenericSemiring rep
  ) => Semiring (UsingGeneric a) where
  add = over2 UsingGeneric genericAdd
  zero = UsingGeneric genericZero
  mul = over2 UsingGeneric genericMul
  one = UsingGeneric genericOne

instance showUsingGeneric ::
  ( Generic a rep
  , GenericShow rep
  ) => Show (UsingGeneric a) where
  show = unwrap >>> genericShow
