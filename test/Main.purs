module Test.Main where

import Prelude
import Data.Generic.Rep as G
import Data.Generic.Rep.Bounded as GBounded
import Data.Generic.Rep.Deep as GDeep
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Ord as GOrd
import Data.Generic.Rep.Show as GShow
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Test.Assert (ASSERT, assert)
import Type.Proxy (Proxy(..))

data List a = Nil | Cons a (List a)

cons :: forall a. a -> List a -> List a
cons = Cons

derive instance genericList :: G.Generic (List a) _

instance eqList :: Eq a => Eq (List a) where
  eq x y = GEq.genericEq x y

instance ordList :: Ord a => Ord (List a) where
  compare x y = GOrd.genericCompare x y

instance showList :: Show a => Show (List a) where
  show x = GShow.genericShow x

instance deepList :: GDeep.Deep a => GDeep.Deep (List a) where
  fromSpine x = GDeep.genericFromSpine x
  toSpine x = GDeep.genericToSpine x
  toSignature x = GDeep.genericToSignature x

data SimpleBounded = A | B | C | D
derive instance genericSimpleBounded :: G.Generic SimpleBounded _
instance eqSimpleBounded :: Eq SimpleBounded where
  eq x y = GEq.genericEq x y
instance ordSimpleBounded :: Ord SimpleBounded where
  compare x y = GOrd.genericCompare x y
instance showSimpleBounded :: Show SimpleBounded where
  show x = GShow.genericShow x
instance boundedSimpleBounded :: Bounded SimpleBounded where
  bottom = GBounded.genericBottom
  top = GBounded.genericTop

main :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
main = do
  logShow (cons 1 (cons 2 Nil))

  logShow (cons 1 (cons 2 Nil) == cons 1 (cons 2 Nil))
  logShow (cons 1 (cons 2 Nil) == cons 1 Nil)

  logShow (cons 1 (cons 2 Nil) `compare` cons 1 (cons 2 Nil))
  logShow (cons 1 (cons 2 Nil) `compare` cons 1 Nil)

  logShow (bottom :: SimpleBounded)
  logShow (top :: SimpleBounded)

  assert
    (GDeep.isValidSpine
      (GDeep.toSignature
        (Proxy :: Proxy (List Int)))
        (GDeep.toSpine (cons 1 (cons 2 Nil))))
