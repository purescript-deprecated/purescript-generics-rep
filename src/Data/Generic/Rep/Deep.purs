module Data.Generic.Rep.Deep
  ( Spine(..)
  , Signature(..)
  , DataConstructor
  , class Deep
  , toSpine
  , toSignature
  , fromSpine
  , isValidSpine
  , genericToSpine
  , genericToSignature
  , genericFromSpine
  , class GenericHasSpine
  , toSpine'
  , fromSpine'
  , class GenericHasArguments
  , toArguments
  , fromArguments
  , argumentCount
  , class GenericHasSignature
  , dataConstructors
  , class GenericHasArgumentSignatures
  , dataConstructorArguments
  ) where

import Prelude
import Control.Alt ((<|>))
import Data.Array (drop, sortBy, take, zipWith)
import Data.Either (Either(..))
import Data.Foldable (all, and, find)
import Data.Function (applyFlipped)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(Constructor), NoArguments(..), NoConstructors, Product(..), Sum(Inr, Inl), from, to)

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Type.Proxy (Proxy(..))

-- | A Spine is a universal representation of an arbitrary data
-- | structure (that does not contain function arrows).
data Spine
  = SProd String (Array (Unit -> Spine))
  | SRecord (Array { recLabel :: String, recValue :: Unit -> Spine })
  | SNumber Number
  | SBoolean Boolean
  | SInt Int
  | SString String
  | SChar Char
  | SArray (Array (Unit -> Spine))
  | SUnit

-- | A Signature is a universal representation of the structure of an
-- | arbitrary data structure (that does not contain function arrows).
data Signature
  = SigProd (Array DataConstructor)
  | SigRecord (Array { recLabel :: String, recValue :: Unit -> Signature })
  | SigNumber
  | SigBoolean
  | SigInt
  | SigString
  | SigChar
  | SigArray (Unit -> Signature)
  | SigUnit

derive instance genericSignature :: Generic Signature _

-- | Identifies a data constructor.
type DataConstructor =
  { sigConstructor :: String
  , sigValues :: Array (Unit -> Signature)
  }

-- | Checks that the spine follows the structure defined by the signature
isValidSpine :: Signature -> Spine -> Boolean
isValidSpine SigBoolean (SBoolean _) = true
isValidSpine SigNumber (SNumber _) = true
isValidSpine SigInt (SInt _) = true
isValidSpine SigString (SString _) = true
isValidSpine SigChar (SChar _) = true
isValidSpine (SigArray sig) (SArray spines) =
  all (isValidSpine (sig unit) <<< applyFlipped unit) spines
isValidSpine (SigProd alts) (SProd tag values) =
  case find (\alt -> alt.sigConstructor == tag) alts of
    Nothing -> false
    Just { sigValues } ->
      and $ zipWith
        (\sig spine -> isValidSpine (sig unit) (spine unit))
        sigValues
        values
isValidSpine (SigRecord fieldSigs) (SRecord fieldVals) =
  and $ zipWith
    (\sig val -> isValidSpine (sig.recValue unit) (val.recValue unit))
    (sortBy (\a b -> compare a.recLabel b.recLabel) fieldSigs)
    (sortBy (\a b -> compare a.recLabel b.recLabel) fieldVals)
isValidSpine SigUnit SUnit = true
isValidSpine _ _ = false

-- | The Deep typeclass provides methods for sending data to/from spine
-- | representations, as well as querying about the signatures of spine
-- | representations.
-- |
-- | For standard data structures, you can simply write
-- | `derive instance deepFoo :: Deep Foo` in the module they are
-- | declared, and the instance methods will be filled in for you.
class Deep a where
  toSignature :: Proxy a -> Signature
  toSpine :: a -> Spine
  fromSpine :: Spine -> Maybe a

instance deepNumber :: Deep Number where
  toSpine = SNumber
  toSignature _ = SigNumber
  fromSpine (SNumber n) = Just n
  fromSpine _ = Nothing

instance deepInt :: Deep Int where
  toSpine = SInt
  toSignature _ = SigInt
  fromSpine (SInt n) = Just n
  fromSpine _ = Nothing

instance deepString :: Deep String where
  toSpine = SString
  toSignature _ = SigString
  fromSpine (SString s) = Just s
  fromSpine _ = Nothing

instance deepChar :: Deep Char where
  toSpine = SChar
  toSignature _ = SigChar
  fromSpine (SChar s) = Just s
  fromSpine _ = Nothing

instance deepBool :: Deep Boolean where
  toSpine = SBoolean
  toSignature _ = SigBoolean
  fromSpine (SBoolean b) = Just b
  fromSpine _ = Nothing

instance deepArray :: Deep a => Deep (Array a) where
  toSpine = SArray <<< map (\x _ -> toSpine x)
  toSignature x = SigArray (\_ -> toSignature (lowerProxy x))
    where
    lowerProxy :: Proxy (Array a) -> Proxy a
    lowerProxy _ = Proxy
  fromSpine (SArray x) = traverse (fromSpine <<< applyFlipped unit) x
  fromSpine _ = Nothing

instance deepUnit :: Deep Unit where
  toSpine _ = SUnit
  toSignature _ = SigUnit
  fromSpine SUnit = Just unit
  fromSpine _ = Nothing

instance deepVoid :: Deep Void where
  toSpine = absurd
  toSignature _ = SigProd []
  fromSpine _ = Nothing

instance deepTuple :: (Deep a, Deep b) => Deep (Tuple a b) where
  toSpine (Tuple x y) =
    SProd "Data.Tuple.Tuple" [\_ -> toSpine x, \_ -> toSpine y]
  toSignature x =
    SigProd
      [ { sigConstructor: "Data.Tuple.Tuple"
        , sigValues:
            [ \_ -> toSignature (fstProxy x)
            , \_ -> toSignature (sndProxy x)
            ]
        }
      ]
    where
    fstProxy :: Proxy (Tuple a b) -> Proxy a
    fstProxy _ = Proxy
    sndProxy :: Proxy (Tuple a b) -> Proxy b
    sndProxy _ = Proxy
  fromSpine (SProd "Data.Tuple.Tuple" [x, y]) =
    Tuple <$> fromSpine (x unit) <*> fromSpine (y unit)
  fromSpine _ = Nothing

instance deepMaybe :: Deep a => Deep (Maybe a) where
  toSpine (Just x) = SProd "Data.Maybe.Just" [\_ -> toSpine x]
  toSpine Nothing = SProd "Data.Maybe.Nothing" []
  toSignature x =
    SigProd
      [ { sigConstructor: "Data.Maybe.Just"
        , sigValues: [\_ -> toSignature (mbProxy x)]
        }
      , { sigConstructor: "Data.Maybe.Nothing"
        , sigValues: []
        }
      ]
    where
    mbProxy :: Proxy (Maybe a) -> Proxy a
    mbProxy _ = Proxy
  fromSpine (SProd "Data.Maybe.Just" [x]) = Just <$> fromSpine (x unit)
  fromSpine (SProd "Data.Maybe.Nothing" []) = pure Nothing
  fromSpine _ = Nothing

instance deepEither :: (Deep a, Deep b) => Deep (Either a b) where
  toSpine (Left x) = SProd "Data.Either.Left" [\_ -> toSpine x]
  toSpine (Right x) = SProd "Data.Either.Right" [\_ -> toSpine x]
  toSignature x =
    SigProd
      [ { sigConstructor: "Data.Either.Left"
        , sigValues: [\_ -> toSignature (lproxy x)]
        }
      , { sigConstructor: "Data.Either.Right"
        , sigValues: [\_ -> toSignature (rproxy x)]
        }
      ]
    where
    lproxy :: Proxy (Either a b) -> Proxy a
    lproxy _ = Proxy
    rproxy :: Proxy (Either a b) -> Proxy b
    rproxy _ = Proxy
  fromSpine (SProd "Data.Either.Left" [x]) = Left <$> fromSpine (x unit)
  fromSpine (SProd "Data.Either.Right" [x]) = Right <$> fromSpine (x unit)
  fromSpine _ = Nothing

instance deepIdentity :: Deep a => Deep (Identity a) where
  toSpine (Identity a) = SProd "Data.Identity.Identity" [\_ -> toSpine a]
  toSignature x =
    SigProd
      [ { sigConstructor: "Data.Identity.Identity"
        , sigValues: [\_ -> toSignature (iproxy x)]
        }
      ]
    where
    iproxy :: Proxy (Identity a) -> Proxy a
    iproxy _ = Proxy
  fromSpine (SProd "Data.Identity.Identity" [x]) = Identity <$> fromSpine (x unit)
  fromSpine _ = Nothing

instance deepOrdering :: Deep Ordering where
  toSpine = case _ of
    LT -> SProd "Data.Ordering.LT" []
    EQ -> SProd "Data.Ordering.EQ" []
    GT -> SProd "Data.Ordering.GT" []
  toSignature _ =
    SigProd
      [ { sigConstructor: "Data.Ordering.LT", sigValues: [] }
      , { sigConstructor: "Data.Ordering.EQ", sigValues: [] }
      , { sigConstructor: "Data.Ordering.GT", sigValues: [] }
      ]
  fromSpine = case _ of
    SProd "Data.Ordering.LT" [] -> Just LT
    SProd "Data.Ordering.EQ" [] -> Just EQ
    SProd "Data.Ordering.GT" [] -> Just GT
    _ -> Nothing

instance deepNonEmpty :: (Deep (f a), Deep a) => Deep (NonEmpty f a) where
  toSpine (NonEmpty x xs) =
    SProd "Data.NonEmpty.NonEmpty" [\_ -> toSpine x, \_ -> toSpine xs]
  toSignature x =
    SigProd
      [ { sigConstructor: "Data.NonEmpty.NonEmpty"
        , sigValues:
            [ \_ -> toSignature (headProxy x)
            , \_ -> toSignature (tailProxy x)
            ]
        }
      ]
    where
    headProxy :: Proxy (NonEmpty f a) -> Proxy a
    headProxy _ = Proxy
    tailProxy :: Proxy (NonEmpty f a) -> Proxy (f a)
    tailProxy _ = Proxy
  fromSpine (SProd "Data.NonEmpty.NonEmpty" [x, xs]) =
    NonEmpty <$> fromSpine (x unit) <*> fromSpine (xs unit)
  fromSpine _ = Nothing

class GenericHasSpine rep where
  toSpine' :: rep -> Spine
  fromSpine' :: Spine -> Maybe rep

instance genericHasSpineNoConstructors :: GenericHasSpine NoConstructors where
  toSpine' x = toSpine' x
  fromSpine' _ = Nothing

instance genericHasSpineSum :: (GenericHasSpine a, GenericHasSpine b) => GenericHasSpine (Sum a b) where
  toSpine' (Inl x) = toSpine' x
  toSpine' (Inr y) = toSpine' y
  fromSpine' x = (Inl <$> fromSpine' x) <|> (Inr <$> fromSpine' x)

instance genericHasSpineConstructor
  :: (GenericHasArguments a, IsSymbol name)
  => GenericHasSpine (Constructor name a) where
  toSpine' (Constructor x) = SProd (reflectSymbol (SProxy :: SProxy name)) (toArguments x)
  fromSpine' (SProd name args)
    | name == reflectSymbol (SProxy :: SProxy name) = Constructor <$> fromArguments args
  fromSpine' _ = Nothing

class GenericHasArguments rep where
  toArguments :: rep -> Array (Unit -> Spine)
  fromArguments :: Array (Unit -> Spine) -> Maybe rep
  argumentCount :: Proxy rep -> Int

instance genericHasArgumentsNoArguments :: GenericHasArguments NoArguments where
  toArguments _ = []
  fromArguments [] = Just NoArguments
  fromArguments _ = Nothing
  argumentCount _ = 0

instance genericHasArgumentsProduct
  :: (GenericHasArguments a, GenericHasArguments b)
  => GenericHasArguments (Product a b) where
  toArguments (Product a b) = toArguments a <> toArguments b
  fromArguments args = Product <$> fromArguments (take n args)
                               <*> fromArguments (drop n args)
    where
      n = argumentCount (Proxy :: Proxy a)
  argumentCount _ = argumentCount (Proxy :: Proxy a) + argumentCount (Proxy :: Proxy b)

instance genericHasArgumentsArgument :: Deep a => GenericHasArguments (Argument a) where
  toArguments (Argument a) = [\_ -> toSpine a]
  fromArguments [x] = Argument <$> fromSpine (x unit)
  fromArguments _ = Nothing
  argumentCount _ = 1

class GenericHasSignature rep where
  dataConstructors :: Proxy rep -> Array DataConstructor

instance genericHasSignatureNoConstructors :: GenericHasSignature NoConstructors where
  dataConstructors _ = []

instance genericHasSignatureSum
  :: (GenericHasSignature a, GenericHasSignature b)
  => GenericHasSignature (Sum a b) where
  dataConstructors _ =
    dataConstructors (Proxy :: Proxy a) <>
    dataConstructors (Proxy :: Proxy b)

instance genericHasSignatureConstructor
  :: (GenericHasArgumentSignatures a, IsSymbol name)
  => GenericHasSignature (Constructor name a) where
  dataConstructors _ =
    [ { sigConstructor: reflectSymbol (SProxy :: SProxy name)
      , sigValues: dataConstructorArguments (Proxy :: Proxy a)
      } ]

class GenericHasArgumentSignatures rep where
  dataConstructorArguments :: Proxy rep -> Array (Unit -> Signature)

instance genericHasArgumentSignaturesNoArguments :: GenericHasArgumentSignatures NoArguments where
  dataConstructorArguments _ = []

instance genericHasArgumentSignaturesProduct
  :: (GenericHasArgumentSignatures a, GenericHasArgumentSignatures b)
  => GenericHasArgumentSignatures (Product a b) where
  dataConstructorArguments _ =
    dataConstructorArguments (Proxy :: Proxy a) <>
    dataConstructorArguments (Proxy :: Proxy b)

instance genericHasArgumentSignaturesArgument
  :: Deep a
  => GenericHasArgumentSignatures (Argument a) where
  dataConstructorArguments _ = [\_ -> toSignature (Proxy :: Proxy a)]

-- | A `Generic` implementation of the `toSpine` member from the `Deep` type class.
genericToSpine :: forall a rep. (Generic a rep, GenericHasSpine rep) => a -> Spine
genericToSpine x = toSpine' (from x)

-- | A `Generic` implementation of the `fromSpine` member from the `Deep` type class.
genericFromSpine :: forall a rep. (Generic a rep, GenericHasSpine rep) => Spine -> Maybe a
genericFromSpine x = to <$> fromSpine' x

-- | A `Generic` implementation of the `toSignature` member from the `Deep` type class.
genericToSignature :: forall a rep. (Generic a rep, GenericHasSignature rep) => Proxy a -> Signature
genericToSignature _ = SigProd (dataConstructors (Proxy :: Proxy rep))
