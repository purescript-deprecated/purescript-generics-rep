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
  , Rec(..)
  , Field(..)
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

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

-- | A representation for records.
newtype Rec fields = Rec fields

-- | A representation for a record field which includes the field name
-- | as a type-level string.
newtype Field (field :: Symbol) a = Field a

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

instance showNoArguments :: Show NoArguments where
  show _ = "NoArguments"

instance showSum :: (Show a, Show b) => Show (Sum a b) where
  show (Inl a) = "(Inl " <> show a <> ")"
  show (Inr b) = "(Inr " <> show b <> ")"

instance showProduct :: (Show a, Show b) => Show (Product a b) where
  show (Product a b) = "(Product " <> show a <> " " <> show b <> ")"

instance showConstructor :: (IsSymbol name, Show a) => Show (Constructor name a) where
  show (Constructor a) = "(Constructor " <> show (reflectSymbol (SProxy :: SProxy name)) <> " " <> show a <> ")"

instance showArgument :: Show a => Show (Argument a) where
  show (Argument a) = "(Argument " <> show a <> ")"

instance showRec :: Show a => Show (Rec a) where
  show (Rec a) = "(Rec " <> show a <> ")"

instance showField :: (IsSymbol name, Show a) => Show (Field name a) where
  show (Field a) = "(Field " <> show (reflectSymbol (SProxy :: SProxy name)) <> " " <> show a <> ")"
