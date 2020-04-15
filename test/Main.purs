module Test.Main where

import Prelude

import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), cardinality, fromEnum, pred, succ, toEnum, enumFromTo)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Derive (UsingGeneric)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.HeytingAlgebra (ff, tt)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Test.Assert (assert)

data List a = Nil | Cons { head :: a, tail :: List a }

cons :: forall a. a -> List a -> List a
cons head tail = Cons { head, tail }

derive instance genericList :: Generic (List a) _

instance eqList :: Eq a => Eq (List a) where
  eq x y = genericEq x y

instance showList :: Show a => Show (List a) where
  show x = genericShow x

data SimpleBounded = A | B | C | D
derive instance genericSimpleBounded :: Generic SimpleBounded _
derive via (UsingGeneric SimpleBounded) instance eqSimpleBounded :: Eq SimpleBounded
derive via (UsingGeneric SimpleBounded) instance ordSimpleBounded :: Ord SimpleBounded
derive via (UsingGeneric SimpleBounded) instance showSimpleBounded :: Show SimpleBounded
derive via (UsingGeneric SimpleBounded) instance boundedSimpleBounded :: Bounded SimpleBounded
derive via (UsingGeneric SimpleBounded) instance enumSimpleBounded :: Enum SimpleBounded
derive via (UsingGeneric SimpleBounded) instance boundedEnumSimpleBounded :: BoundedEnum SimpleBounded

data Option a = None | Some a
derive instance genericOption :: Generic (Option a) _
derive via (UsingGeneric (Option a)) instance eqOption :: Eq a => Eq (Option a)
derive via (UsingGeneric (Option a)) instance ordOption :: Ord a => Ord (Option a)
derive via (UsingGeneric (Option a)) instance showOption :: Show a => Show (Option a)
derive via (UsingGeneric (Option a)) instance boundedOption :: Bounded a => Bounded (Option a)
derive via (UsingGeneric (Option a)) instance enumOption :: (Bounded a, Enum a) => Enum (Option a)
derive via (UsingGeneric (Option a)) instance boundedEnumOption :: BoundedEnum a => BoundedEnum (Option a)

data Bit = Zero | One
derive instance genericBit :: Generic Bit _
derive via (UsingGeneric Bit) instance eqBit :: Eq Bit
derive via (UsingGeneric Bit) instance ordBit :: Ord Bit
derive via (UsingGeneric Bit) instance showBit :: Show Bit
derive via (UsingGeneric Bit) instance boundedBit :: Bounded Bit
derive via (UsingGeneric Bit) instance enumBit :: Enum Bit
derive via (UsingGeneric Bit) instance boundedEnumBit :: BoundedEnum Bit

data Pair a b = Pair a b
derive instance genericPair :: Generic (Pair a b) _
derive via (UsingGeneric (Pair a b)) instance eqPair :: (Eq a, Eq b) => Eq (Pair a b)
derive via (UsingGeneric (Pair a b)) instance ordPair :: (Ord a, Ord b) => Ord (Pair a b)
derive via (UsingGeneric (Pair a b)) instance showPair :: (Show a, Show b) => Show (Pair a b)
derive via (UsingGeneric (Pair a b)) instance boundedPair :: (Bounded a, Bounded b) => Bounded (Pair a b)
derive via (UsingGeneric (Pair a b)) instance enumPair :: (Bounded a, Enum a, Bounded b, Enum b) => Enum (Pair a b)
derive via (UsingGeneric (Pair a b)) instance boundedEnumPair :: (BoundedEnum a, BoundedEnum b) => BoundedEnum (Pair a b)

data A1 = A1 (Tuple (Tuple Int {a :: Int}) {a :: Int})
derive instance genericA1 :: Generic A1 _
derive via (UsingGeneric A1) instance eqA1 :: Eq A1
derive via (UsingGeneric A1) instance showA1 :: Show A1
derive via (UsingGeneric A1) instance semiringA1 :: Semiring A1
derive via (UsingGeneric A1) instance ringA1 :: Ring A1

data B1 = B1 (Tuple (Tuple Boolean {a :: Boolean}) {a :: Boolean})
derive instance genericB1 :: Generic B1 _
derive via (UsingGeneric B1) instance eqB1 :: Eq B1
derive via (UsingGeneric B1) instance showB1 :: Show B1
derive via (UsingGeneric B1) instance heytingAlgebraB1 :: HeytingAlgebra B1

instance booleanAlgebraB1 :: BooleanAlgebra B1

main :: Effect Unit
main = do
  logShow (cons 1 (cons 2 Nil))

  log "Checking equality"
  assert $ cons 1 (cons 2 Nil) == cons 1 (cons 2 Nil)

  log "Checking inequality"
  assert $ cons 1 (cons 2 Nil) /= cons 1 Nil

  log "Checking comparison EQ"
  assert $ (Pair Zero (Some One) `compare` Pair Zero (Some One)) == EQ

  log "Checking comparison GT"
  assert $ (Pair (Some One) Zero `compare` Pair (Some Zero) Zero) == GT

  log "Checking comparison LT"
  assert $ (Pair Zero One `compare` Pair One One) == LT

  log "Checking simple bottom"
  assert $ bottom == A

  log "Checking simple top"
  assert $ top == D

  log "Checking composite bottom"
  assert $ bottom == (None :: Option SimpleBounded)

  log "Checking composite top"
  assert $ top == Some D

  log "Checking product bottom"
  assert $ bottom == Pair Zero A

  log "Checking product top"
  assert $ top == Pair One D

  log "Checking simple pred bottom"
  assert $ pred (bottom :: SimpleBounded) == Nothing

  log "Checking simple (pred =<< succ bottom)"
  assert $ (pred =<< succ bottom) == Just A

  log "Checking simple succ top"
  assert $ succ (top :: SimpleBounded) == Nothing

  log "Checking simple (succ =<< pred top)"
  assert $ (succ =<< pred top) == Just D

  log "Checking composite pred bottom"
  assert $ pred (bottom :: Option SimpleBounded) == Nothing

  log "Checking composite (pred =<< succ bottom)"
  assert $ (pred =<< succ (bottom :: Option SimpleBounded)) == Just None

  log "Checking composite succ top"
  assert $ succ (top :: Option SimpleBounded) == Nothing

  log "Checking composite (succ =<< pred top)"
  assert $ (succ =<< pred top) == Just (Some D)

  log "Checking product pred bottom"
  assert $ pred (bottom :: Pair Bit SimpleBounded) == Nothing

  log "Checking product (pred =<< succ bottom)"
  assert $ (pred =<< succ (bottom :: Pair Bit SimpleBounded)) == Just (Pair Zero A)

  log "Checking product succ top"
  assert $ succ (top :: Pair Bit SimpleBounded) == Nothing

  log "Checking product (succ =<< pred top)"
  assert $ (succ =<< pred top) == Just (Pair One D)

  log "Checking simple cardinality"
  assert $ (cardinality :: Cardinality SimpleBounded) == Cardinality 4

  log "Checking composite cardinality"
  assert $ (cardinality :: Cardinality (Option SimpleBounded)) == Cardinality 5

  log "Checking product cardinality"
  assert $ (cardinality :: Cardinality (Pair Bit SimpleBounded)) == Cardinality 8

  log "Checking simple toEnum/fromEnum roundtrip"
  assert $ toEnum (fromEnum A) == Just A
  assert $ toEnum (fromEnum B) == Just B

  log "Checking composite toEnum/fromEnum roundtrip"
  assert $ toEnum (fromEnum (None :: Option SimpleBounded)) == Just (None :: Option SimpleBounded)
  assert $ toEnum (fromEnum (Some A)) == Just (Some A)

  log "Checking product toEnum/fromEnum roundtrip"
  assert $ let allPairs = enumFromTo bottom top :: Array (Pair Bit SimpleBounded)
           in (toEnum <<< fromEnum <$> allPairs) == (Just <$> allPairs)

  log "Checking zero"
  assert $ (zero :: A1) == A1 (Tuple (Tuple 0 {a: 0}) {a: 0})

  log "Checking one"
  assert $ (one :: A1) == A1 (Tuple (Tuple 1 {a: 1}) {a: 1})

  log "Checking add"
  assert $ A1 (Tuple (Tuple 100 {a: 10}) {a: 20}) + A1 (Tuple (Tuple 50 {a: 30}) {a: 40}) == A1 (Tuple (Tuple 150 {a: 40}) {a: 60})

  log "Checking mul"
  assert $ A1 (Tuple (Tuple 100 {a: 10}) {a: 20}) * A1 (Tuple (Tuple 50 {a: 30}) {a: 40}) == A1 (Tuple (Tuple 5000 {a: 300}) {a: 800})

  log "Checking sub"
  assert $ A1 (Tuple (Tuple 100 {a: 10}) {a: 20}) - A1 (Tuple (Tuple 50 {a: 30}) {a: 40}) == A1 (Tuple (Tuple 50 {a: -20}) {a: -20})

  log "Checking ff"
  assert $ (ff :: B1) == B1 (Tuple (Tuple false {a: false}) {a: false})

  log "Checking tt"
  assert $ (tt :: B1) == B1 (Tuple (Tuple true {a: true}) {a: true})

  log "Checking conj"
  assert $ (B1 (Tuple (Tuple true {a: false}) {a: true}) && B1 (Tuple (Tuple false {a: false}) {a: true})) == B1 (Tuple (Tuple false { a: false }) { a: true })

  log "Checking disj"
  assert $ (B1 (Tuple (Tuple true {a: false}) {a: true}) || B1 (Tuple (Tuple false {a: false}) {a: true})) == B1 (Tuple (Tuple true { a: false }) { a: true })

  log "Checking not"
  assert $ not B1 (Tuple (Tuple true {a: false}) {a: true}) == B1 (Tuple (Tuple false {a: true}) {a: false})
