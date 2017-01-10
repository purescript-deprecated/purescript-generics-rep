module Data.Generic.Rep.Bounded
  ( class GenericBottom
  , genericBottom'
  , genericBottom
  , class GenericTop
  , genericTop'
  , genericTop
  ) where

import Prelude (class Bounded, bottom, top)
import Data.Generic.Rep

class GenericBottom a where
  genericBottom' :: a
instance genericBottomNoArguments :: GenericBottom NoArguments where
  genericBottom' = NoArguments

instance genericBottomSum :: GenericBottom a => GenericBottom (Sum a b) where
  genericBottom' = Inl genericBottom'

instance genericBottomConstructor :: GenericBottom a => GenericBottom (Constructor name a) where
  genericBottom' = Constructor genericBottom'

instance genericBottomProduct :: (GenericBottom a, GenericBottom b) => GenericBottom (Product a b) where
  genericBottom' = Product genericBottom' genericBottom'

instance genericBottomArgument :: Bounded a => GenericBottom (Argument a) where
  genericBottom' = Argument bottom

instance genericBottomRec :: GenericBottom a => GenericBottom (Rec a) where
  genericBottom' = Rec genericBottom'

instance genericBottomField :: GenericBottom a => GenericBottom (Field name a) where
  genericBottom' = Field genericBottom'

class GenericTop a where
  genericTop' :: a

instance genericTopNoArguments :: GenericTop NoArguments where
  genericTop' = NoArguments

instance genericTopSum :: GenericTop b => GenericTop (Sum a b) where
  genericTop' = Inr genericTop'

instance genericTopConstructor :: GenericTop a => GenericTop (Constructor name a) where
  genericTop' = Constructor genericTop'

instance genericTopProduct :: (GenericTop a, GenericTop b) => GenericTop (Product a b) where
  genericTop' = Product genericTop' genericTop'

instance genericTopArgument :: Bounded a => GenericTop (Argument a) where
  genericTop' = Argument top

instance genericTopRec :: GenericTop a => GenericTop (Rec a) where
  genericTop' = Rec genericTop'

instance genericTopField :: GenericTop a => GenericTop (Field name a) where
  genericTop' = Field genericTop'

-- | A `Generic` implementation of the `bottom` member from the `Bounded` type class.
genericBottom :: forall a rep. (Generic a rep, GenericBottom rep) => a
genericBottom = to genericBottom'

-- | A `Generic` implementation of the `top` member from the `Bounded` type class.
genericTop :: forall a rep. (Generic a rep, GenericTop rep) => a
genericTop = to genericTop'
