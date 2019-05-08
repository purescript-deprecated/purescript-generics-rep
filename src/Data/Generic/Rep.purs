module Data.Generic.Rep
  ( class Generic
  , to
  , from
  , NoConstructors
  , NoArguments(..)
  , Sum(..)
  , Product(..)
  , Constructor(..)
  , Argument(..)
  ) where

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

-- | A representation for types with no constructors.
data NoConstructors

-- | A representation for constructors with no arguments.
data NoArguments = NoArguments

-- | A representation for types with multiple constructors.
data Sum a b = Inl a | Inr b

-- | A representation for constructors with multiple fields.
data Product a b = Product a b

-- | A representation for constructors which includes the data constructor name
-- | as a type-level string.
newtype Constructor (name :: Symbol) a = Constructor a

-- | A representation for an argument in a data constructor.
newtype Argument a = Argument a

-- | The `Generic` class asserts the existence of a type function from types
-- | to their representations using the type constructors defined in this module.
class Generic a rep | a -> rep where
  to :: rep -> a
  from :: a -> rep

instance genericMaybe
  :: Generic (Maybe a) (Sum (Constructor "Nothing" NoArguments)
                            (Constructor "Just" (Argument a))) where
  to (Inl _) = Nothing
  to (Inr (Constructor (Argument a))) = Just a

  from Nothing = Inl (Constructor NoArguments)
  from (Just a) = Inr (Constructor (Argument a))

instance genericEither
  :: Generic (Either a b) (Sum (Constructor "Left" (Argument a))
                               (Constructor "Right" (Argument b))) where
  to (Inl (Constructor (Argument a))) = Left a
  to (Inr (Constructor (Argument b))) = Right b

  from (Left a) = Inl (Constructor (Argument a))
  from (Right b) = Inr (Constructor (Argument b))