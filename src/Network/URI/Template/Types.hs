{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Network.URI.Template.Types where

import Control.Arrow (Arrow ((***)))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Kind (Type)
import Data.Fixed (Fixed, HasResolution)
import Data.Foldable as F (Foldable (toList))
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import qualified Data.HashMap.Strict as HS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as MS
import Data.Monoid (All, Any, Dual, First, Last, Product, Sum)
import Data.Semigroup (Max, Min)
import qualified Data.String as S
import Data.Tagged (Tagged, unTagged)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (
  Day,
  DayOfWeek,
  LocalTime,
  NominalDiffTime,
  TimeOfDay,
  UTCTime,
  ZonedTime,
 )
import Data.Time.Calendar.Month.Compat (Month)
import Data.Time.Calendar.Quarter.Compat (Quarter, QuarterOfYear)
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as V
import Data.Version (Version)
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Natural (Natural)
import Web.Cookie (SetCookie)
import Web.HttpApiData (ToHttpApiData (toUrlPiece))


data Single


data Associative


data List


-- | All values must reduce to a single value pair, an associative list of keys and values, or a list of values without keys.
data TemplateValue a where
  Single :: ToHttpApiData a => a -> TemplateValue Single
  Associative :: [(TemplateValue Single, TemplateValue Single)] -> TemplateValue Associative
  List :: [TemplateValue Single] -> TemplateValue List


instance Show (TemplateValue a) where
  show (Single s) = "Single " ++ show (toUrlPiece s)
  show (Associative as) = "Associative [" ++ intercalate ", " (map formatTuple as) ++ "]"
   where
    formatTuple (k, v) = "(" ++ show k ++ ", " ++ show v ++ ")"
  show (List s) = "List [" ++ intercalate ", " (map show s) ++ "]"


instance Semigroup (TemplateValue Associative) where
  Associative l <> Associative r = Associative (l <> r)


instance Monoid (TemplateValue Associative) where
  mempty = Associative mempty


instance Semigroup (TemplateValue List) where
  List l <> List r = List (l <> r)


instance Monoid (TemplateValue List) where
  mempty = List []


data WrappedValue where
  WrappedValue :: TemplateValue a -> WrappedValue


-- | A simple wrapper for interpolating Haskell 98 strings into templates.
newtype TemplateString = String {fromString :: String}
  deriving (Read, Show, Eq, S.IsString, ToHttpApiData)


{- | A simple list of key value pairs. Useful when you want to be able to have multiple duplicate
 keys, which 'Map' and 'HashMap' don't support.
-}
newtype AList k v = AList
  { fromAList :: [(k, v)]
  }
  deriving (Read, Show, Eq, Ord, Semigroup, Monoid)


instance Functor (AList k) where
  fmap f (AList l) = AList $ fmap (fmap f) l


instance Bifunctor AList where
  bimap f g (AList l) = AList $ map (bimap f g) l


class ToTemplateValue a where
  type TemplateRep a :: Type
  type TemplateRep a = Single
  toTemplateValue :: a -> TemplateValue (TemplateRep a)
  default toTemplateValue :: (ToHttpApiData a, TemplateRep a ~ Single) => a -> TemplateValue (TemplateRep a)
  toTemplateValue = Single


instance ToTemplateValue Bool


instance ToTemplateValue Char


instance ToTemplateValue Double


instance ToTemplateValue Float


instance ToTemplateValue Int


instance ToTemplateValue Int8


instance ToTemplateValue Int16


instance ToTemplateValue Int32


instance ToTemplateValue Int64


instance ToTemplateValue Integer


instance ToTemplateValue Ordering


instance ToTemplateValue Natural


instance ToTemplateValue Word


instance ToTemplateValue Word8


instance ToTemplateValue Word16


instance ToTemplateValue Word32


instance ToTemplateValue Word64


instance ToTemplateValue ()


instance ToTemplateValue T.Text


instance ToTemplateValue TL.Text


instance ToTemplateValue TemplateString


instance ToTemplateValue Void


instance ToTemplateValue Version


instance ToTemplateValue All


instance ToTemplateValue Any


instance ToTemplateValue UTCTime


instance ToTemplateValue SetCookie


instance ToTemplateValue UUID.UUID


-- TODO these can probably be provided for things where TemplateRep a isn't Single too...
instance (ToTemplateValue a, TemplateRep a ~ Single, ToHttpApiData a) => ToTemplateValue (Min a)


instance (ToTemplateValue a, TemplateRep a ~ Single, ToHttpApiData a) => ToTemplateValue (Max a)


instance (ToTemplateValue a, TemplateRep a ~ Single, ToHttpApiData a) => ToTemplateValue (First a)


instance (ToTemplateValue a, TemplateRep a ~ Single, ToHttpApiData a) => ToTemplateValue (Last a)


instance (ToTemplateValue a, TemplateRep a ~ Single, ToHttpApiData a) => ToTemplateValue (Identity a)


instance (ToTemplateValue a, TemplateRep a ~ Single, ToHttpApiData a) => ToTemplateValue (Dual a)


instance (ToTemplateValue a, TemplateRep a ~ Single, ToHttpApiData a) => ToTemplateValue (Sum a)


instance (ToTemplateValue a, TemplateRep a ~ Single, ToHttpApiData a) => ToTemplateValue (Product a)


instance (ToTemplateValue a, TemplateRep a ~ Single, ToHttpApiData a, HasResolution a) => ToTemplateValue (Fixed a)


instance (ToTemplateValue a, TemplateRep a ~ Single, ToHttpApiData a) => ToTemplateValue (Const a b)


instance (ToTemplateValue a, TemplateRep a ~ Single, ToHttpApiData a) => ToTemplateValue (Tagged b a) where
  toTemplateValue = toTemplateValue . unTagged


instance ToTemplateValue ZonedTime


instance ToTemplateValue LocalTime


instance ToTemplateValue TimeOfDay


instance ToTemplateValue NominalDiffTime


instance ToTemplateValue DayOfWeek


instance ToTemplateValue Day


instance ToTemplateValue QuarterOfYear


instance ToTemplateValue Quarter


instance ToTemplateValue Month


instance (ToTemplateValue a, ToTemplateValue b, TemplateRep a ~ Single, TemplateRep b ~ Single, ToHttpApiData a, ToHttpApiData b) => ToTemplateValue (Either a b)


instance (ToTemplateValue a, TemplateRep a ~ Single) => ToTemplateValue [a] where
  type TemplateRep [a] = List
  toTemplateValue = List . map toTemplateValue


instance (ToTemplateValue a, TemplateRep a ~ Single) => ToTemplateValue (NE.NonEmpty a) where
  type TemplateRep (NE.NonEmpty a) = List
  toTemplateValue = toTemplateValue . NE.toList


instance (ToTemplateValue k, TemplateRep k ~ Single, ToTemplateValue v, TemplateRep v ~ Single) => ToTemplateValue (AList k v) where
  type TemplateRep (AList k v) = Associative
  toTemplateValue = Associative . map (toTemplateValue *** toTemplateValue) . fromAList


instance (ToTemplateValue a, TemplateRep a ~ Single) => ToTemplateValue (V.Vector a) where
  type TemplateRep (V.Vector a) = List
  toTemplateValue = List . F.toList . fmap toTemplateValue


instance (ToTemplateValue a, TemplateRep a ~ Single, ToHttpApiData a) => ToTemplateValue (Maybe a)


instance (ToTemplateValue k, TemplateRep k ~ Single, ToTemplateValue v, TemplateRep v ~ Single) => ToTemplateValue (HS.HashMap k v) where
  type TemplateRep (HS.HashMap k v) = Associative
  toTemplateValue = toTemplateValue . AList . HS.toList


instance (ToTemplateValue k, TemplateRep k ~ Single, ToTemplateValue v, TemplateRep v ~ Single) => ToTemplateValue (MS.Map k v) where
  type TemplateRep (MS.Map k v) = Associative
  toTemplateValue = toTemplateValue . AList . MS.toList


instance
  ( ToTemplateValue a
  , TemplateRep a ~ Single
  , ToTemplateValue b
  , TemplateRep b ~ Single
  ) =>
  ToTemplateValue (a, b)
  where
  type TemplateRep (a, b) = List
  toTemplateValue (a, b) =
    List
      [ toTemplateValue a
      , toTemplateValue b
      ]


instance
  ( ToTemplateValue a
  , TemplateRep a ~ Single
  , ToTemplateValue b
  , TemplateRep b ~ Single
  , ToTemplateValue c
  , TemplateRep c ~ Single
  ) =>
  ToTemplateValue (a, b, c)
  where
  type TemplateRep (a, b, c) = List
  toTemplateValue (a, b, c) =
    List
      [ toTemplateValue a
      , toTemplateValue b
      , toTemplateValue c
      ]


instance
  ( ToTemplateValue a
  , TemplateRep a ~ Single
  , ToTemplateValue b
  , TemplateRep b ~ Single
  , ToTemplateValue c
  , TemplateRep c ~ Single
  , ToTemplateValue d
  , TemplateRep d ~ Single
  ) =>
  ToTemplateValue (a, b, c, d)
  where
  type TemplateRep (a, b, c, d) = List
  toTemplateValue (a, b, c, d) =
    List
      [ toTemplateValue a
      , toTemplateValue b
      , toTemplateValue c
      , toTemplateValue d
      ]


instance
  ( ToTemplateValue a
  , TemplateRep a ~ Single
  , ToTemplateValue b
  , TemplateRep b ~ Single
  , ToTemplateValue c
  , TemplateRep c ~ Single
  , ToTemplateValue d
  , TemplateRep d ~ Single
  , ToTemplateValue e
  , TemplateRep e ~ Single
  ) =>
  ToTemplateValue (a, b, c, d, e)
  where
  type TemplateRep (a, b, c, d, e) = List
  toTemplateValue (a, b, c, d, e) =
    List
      [ toTemplateValue a
      , toTemplateValue b
      , toTemplateValue c
      , toTemplateValue d
      , toTemplateValue e
      ]


instance
  ( ToTemplateValue a
  , TemplateRep a ~ Single
  , ToTemplateValue b
  , TemplateRep b ~ Single
  , ToTemplateValue c
  , TemplateRep c ~ Single
  , ToTemplateValue d
  , TemplateRep d ~ Single
  , ToTemplateValue e
  , TemplateRep e ~ Single
  , ToTemplateValue f
  , TemplateRep f ~ Single
  ) =>
  ToTemplateValue (a, b, c, d, e, f)
  where
  type TemplateRep (a, b, c, d, e, f) = List
  toTemplateValue (a, b, c, d, e, f) =
    List
      [ toTemplateValue a
      , toTemplateValue b
      , toTemplateValue c
      , toTemplateValue d
      , toTemplateValue e
      , toTemplateValue f
      ]


instance
  ( ToTemplateValue a
  , TemplateRep a ~ Single
  , ToTemplateValue b
  , TemplateRep b ~ Single
  , ToTemplateValue c
  , TemplateRep c ~ Single
  , ToTemplateValue d
  , TemplateRep d ~ Single
  , ToTemplateValue e
  , TemplateRep e ~ Single
  , ToTemplateValue f
  , TemplateRep f ~ Single
  , ToTemplateValue g
  , TemplateRep g ~ Single
  ) =>
  ToTemplateValue (a, b, c, d, e, f, g)
  where
  type TemplateRep (a, b, c, d, e, f, g) = List
  toTemplateValue (a, b, c, d, e, f, g) =
    List
      [ toTemplateValue a
      , toTemplateValue b
      , toTemplateValue c
      , toTemplateValue d
      , toTemplateValue e
      , toTemplateValue f
      , toTemplateValue g
      ]


instance
  ( ToTemplateValue a
  , TemplateRep a ~ Single
  , ToTemplateValue b
  , TemplateRep b ~ Single
  , ToTemplateValue c
  , TemplateRep c ~ Single
  , ToTemplateValue d
  , TemplateRep d ~ Single
  , ToTemplateValue e
  , TemplateRep e ~ Single
  , ToTemplateValue f
  , TemplateRep f ~ Single
  , ToTemplateValue g
  , TemplateRep g ~ Single
  , ToTemplateValue h
  , TemplateRep h ~ Single
  ) =>
  ToTemplateValue (a, b, c, d, e, f, g, h)
  where
  type TemplateRep (a, b, c, d, e, f, g, h) = List
  toTemplateValue (a, b, c, d, e, f, g, h) =
    List
      [ toTemplateValue a
      , toTemplateValue b
      , toTemplateValue c
      , toTemplateValue d
      , toTemplateValue e
      , toTemplateValue f
      , toTemplateValue g
      , toTemplateValue h
      ]


instance
  ( ToTemplateValue a
  , TemplateRep a ~ Single
  , ToTemplateValue b
  , TemplateRep b ~ Single
  , ToTemplateValue c
  , TemplateRep c ~ Single
  , ToTemplateValue d
  , TemplateRep d ~ Single
  , ToTemplateValue e
  , TemplateRep e ~ Single
  , ToTemplateValue f
  , TemplateRep f ~ Single
  , ToTemplateValue g
  , TemplateRep g ~ Single
  , ToTemplateValue h
  , TemplateRep h ~ Single
  , ToTemplateValue i
  , TemplateRep i ~ Single
  ) =>
  ToTemplateValue (a, b, c, d, e, f, g, h, i)
  where
  type TemplateRep (a, b, c, d, e, f, g, h, i) = List
  toTemplateValue (a, b, c, d, e, f, g, h, i) =
    List
      [ toTemplateValue a
      , toTemplateValue b
      , toTemplateValue c
      , toTemplateValue d
      , toTemplateValue e
      , toTemplateValue f
      , toTemplateValue g
      , toTemplateValue h
      , toTemplateValue i
      ]


data ValueModifier
  = Normal
  | Explode
  | MaxLength Int
  deriving (Read, Show, Eq)


data Variable = Variable
  { variableName :: T.Text
  , variableValueModifier :: ValueModifier
  }
  deriving (Read, Show, Eq)


data TemplateSegment
  = -- | A literal string. No URI escaping will be performed
    Literal T.Text
  | -- | An interpolation can have multiple variables (separated by commas in the textual format)
    Embed Modifier [Variable]
  deriving (Read, Eq)


instance Show TemplateSegment where
  show (Literal t) = T.unpack t
  show (Embed mod vars) = "{" ++ modifierPrefix mod ++ intercalate "," (map showVariable vars) ++ "}"
    where
      showVariable (Variable name valueMod) = T.unpack name ++ showValueModifier valueMod
      showValueModifier Normal = ""
      showValueModifier Explode = "*"
      showValueModifier (MaxLength n) = ":" ++ show n
      modifierPrefix Simple = ""
      modifierPrefix Reserved = "+"
      modifierPrefix Fragment = "#"
      modifierPrefix Label = "."
      modifierPrefix PathSegment = "/"
      modifierPrefix PathParameter = ";"
      modifierPrefix Query = "?"
      modifierPrefix QueryContinuation = "&"


{- | A URI template is fundamentally a bunch of segments that are either constants
 or else an interpolation
-}
newtype UriTemplate = UriTemplate
  { uriTemplateSegments :: [TemplateSegment]
  }
  deriving (Read, Eq)


instance Show UriTemplate where
  show = renderTemplate


-- | Render a 'UriTemplate' back to its RFC 6570 string representation.
--
-- This is useful for debugging, logging, or storing templates as strings.
--
-- >>> renderTemplate (UriTemplate [Literal "http://example.com/", Embed PathSegment [Variable "path" Normal]])
-- "http://example.com/{/path}"
renderTemplate :: UriTemplate -> String
renderTemplate = concatMap show . uriTemplateSegments


-- | How an interpolated value should be rendered
data Modifier
  = -- | No prefix
    Simple
  | -- | Prefixed by @+@
    Reserved
  | -- | Prefixed by @#@
    Fragment
  | -- | Prefixed by @.@
    Label
  | -- | Prefixed by @/@
    PathSegment
  | -- | Prefixed by @;@
    PathParameter
  | -- | Prefixed by @?@
    Query
  | -- | Prefixed by @&@
    QueryContinuation
  deriving (Read, Show, Eq)
