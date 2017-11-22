module Data.Generic.Rep.Show
  ( class GenericShow
  , genericShow'
  , genericShow
  , class GenericShowArgs
  , genericShowArgs
  , class GenericShowFields
  , genericShowFields
  ) where

import Prelude (class Show, show, (<>))
import Data.Foldable (intercalate)
import Data.Generic.Rep
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

class GenericShow a where
  genericShow' :: a -> String

instance genericShowNoConstructors :: GenericShow NoConstructors where
  genericShow' a = genericShow' a

instance genericShowNoArgs :: GenericShow NoArguments where
  genericShow' _ = ""

instance genericShowSum :: (GenericShow a, GenericShow b) => GenericShow (Sum a b) where
  genericShow' (Inl l) = genericShow' l
  genericShow' (Inr r) = genericShow' r

instance genericShowProduct
  :: (GenericShow a, GenericShow b)
  => GenericShow (Product a b)
  where
    genericShow' (Product l r) = (genericShow' l) <> ", " <> (genericShow' r)

instance genericShowConstructor
  :: (GenericShow a, IsSymbol name)
  => GenericShow (Constructor name a)
  where
    genericShow' (Constructor a) =
      case genericShow' a of
        ""   -> ctor
        args -> "(" <> (ctor <> args) <> ")"
      where
        ctor :: String
        ctor = reflectSymbol (SProxy :: SProxy name)

instance genericShowArgument :: Show a => GenericShow (Argument a) where
  genericShow' (Argument a) = show a

instance genericShowRec :: (GenericShow a) => GenericShow (Rec a) where
  genericShow' (Rec a) = "{ " <> (genericShow' a) <> " }"

instance genericShowFieldsField :: (Show a, IsSymbol name) => GenericShow (Field name a) where
  genericShow' (Field a) = reflectSymbol (SProxy :: SProxy name) <> ": " <> show a

-- | A `Generic` implementation of the `show` member from the `Show` type class.
genericShow :: forall a rep. Generic a rep => GenericShow rep => a -> String
genericShow o = genericShow' (from o)

