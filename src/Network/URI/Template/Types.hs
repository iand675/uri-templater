{-# LANGUAGE EmptyDataDecls, GADTs, FunctionalDependencies, MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances, FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving #-}
module Network.URI.Template.Types where
import Control.Arrow
import Data.Foldable as F
import Data.Functor.Identity
import Data.List
import qualified Data.String as S
import qualified Data.HashMap.Strict as HS
import Data.Int
import Data.Word
import qualified Data.Map.Strict as MS
import Data.Monoid
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time
import Data.Version
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as V
import Numeric.Natural

data Single
data Associative
data List

-- | All values must reduce to a single value pair, an associative list of keys and values, or a list of values without keys.
data TemplateValue a where
  Single :: String -> TemplateValue Single
  Associative :: [(TemplateValue Single, TemplateValue Single)] -> TemplateValue Associative
  List :: [TemplateValue Single] -> TemplateValue List

instance Show (TemplateValue a) where
  show (Single s) = "Single " ++ s
  show (Associative as) = "Associative [" ++ intercalate ", " (map formatTuple as) ++ "]"
    where
      formatTuple (k, v) = "(" ++ show k ++ ", " ++ show v ++ ")"
  show (List s) = "List [" ++ intercalate ", " (map show s) ++ "]"

data WrappedValue where
  WrappedValue :: TemplateValue a -> WrappedValue

-- | A simple wrapper for interpolating Haskell 98 strings into templates.
newtype TemplateString = String { fromString :: String }
  deriving (Read, Show, Eq, S.IsString)

-- | A simple list of key value pairs. Useful when you want to be able to have multiple duplicate
-- keys, which 'Map' and 'HashMap' don't support.
newtype AList k v = AList
  { fromAList :: [(k, v)]
  } deriving (Read, Show, Eq)

class ToTemplateValue a where
  type TemplateRep a :: *
  type TemplateRep a = Single
  toTemplateValue :: a -> TemplateValue (TemplateRep a)

instance ToTemplateValue () where
  toTemplateValue = const $ Single "_"

instance ToTemplateValue Bool where
  toTemplateValue = Single . show

instance ToTemplateValue Int where
  toTemplateValue = Single . show

instance ToTemplateValue Integer where
  toTemplateValue = Single . show

instance ToTemplateValue Natural where
  toTemplateValue = Single . show

instance ToTemplateValue Double where
  toTemplateValue = Single . show

instance ToTemplateValue Float where
  toTemplateValue = Single . show

instance ToTemplateValue Int8 where
  toTemplateValue = Single . show

instance ToTemplateValue Int16 where
  toTemplateValue = Single . show

instance ToTemplateValue Int32 where
  toTemplateValue = Single . show

instance ToTemplateValue Int64 where
  toTemplateValue = Single . show

instance ToTemplateValue Word where
  toTemplateValue = Single . show

instance ToTemplateValue Word8 where
  toTemplateValue = Single . show

instance ToTemplateValue Word16 where
  toTemplateValue = Single . show

instance ToTemplateValue Word32 where
  toTemplateValue = Single . show

instance ToTemplateValue Word64 where
  toTemplateValue = Single . show

instance ToTemplateValue TemplateString where
  toTemplateValue = Single . fromString

instance (ToTemplateValue a, TemplateRep a ~ Single) => ToTemplateValue (Dual a) where
  toTemplateValue = toTemplateValue . getDual

instance (ToTemplateValue a, TemplateRep a ~ Single) => ToTemplateValue (Sum a) where
  toTemplateValue = toTemplateValue . getSum

instance (ToTemplateValue a, TemplateRep a ~ Single) => ToTemplateValue (Product a) where
  toTemplateValue = toTemplateValue . getProduct

instance (ToTemplateValue a, TemplateRep a ~ Single) => ToTemplateValue (First a) where
  toTemplateValue = toTemplateValue . getFirst

instance (ToTemplateValue a, TemplateRep a ~ Single) => ToTemplateValue (Last a) where
  toTemplateValue = toTemplateValue . getLast

instance ToTemplateValue All where
  toTemplateValue = toTemplateValue . getAll

instance ToTemplateValue Any where
  toTemplateValue = toTemplateValue . getAny

instance ToTemplateValue UUID.UUID where
  toTemplateValue = Single . UUID.toString

timeToString :: FormatTime t => String -> t -> String
timeToString fmt = formatTime defaultTimeLocale (iso8601DateFormat (Just fmt))

instance ToTemplateValue UTCTime where
  toTemplateValue = Single . timeToString "%H:%M:%S%QZ"

instance ToTemplateValue NominalDiffTime where
  toTemplateValue = toTemplateValue . (floor :: NominalDiffTime -> Integer)

instance ToTemplateValue LocalTime where
  toTemplateValue = Single . timeToString "%H:%M:%S%Q"

instance ToTemplateValue ZonedTime where
  toTemplateValue = Single . timeToString "%H:%M:%S%Q%z"

instance ToTemplateValue TimeOfDay where
  toTemplateValue = Single . formatTime defaultTimeLocale "%H:%M:%S%Q"

instance ToTemplateValue Day where
  toTemplateValue = Single . show

instance ToTemplateValue Version where
  toTemplateValue = Single . showVersion

instance ToTemplateValue Ordering where
  toTemplateValue = Single . show

instance (ToTemplateValue a, ToTemplateValue b, TemplateRep a ~ Single, TemplateRep b ~ Single) => ToTemplateValue (Either a b) where
  toTemplateValue = either toTemplateValue toTemplateValue

instance (ToTemplateValue a, (TemplateRep a) ~ Single) => ToTemplateValue [a] where
  type TemplateRep [a] = List
  toTemplateValue = List . map toTemplateValue

instance (ToTemplateValue a, TemplateRep a ~ Single) => ToTemplateValue (NE.NonEmpty a) where
  type TemplateRep (NE.NonEmpty a) = List
  toTemplateValue = toTemplateValue . NE.toList

instance (ToTemplateValue k, (TemplateRep k) ~ Single, ToTemplateValue v, (TemplateRep v) ~ Single) => ToTemplateValue (AList k v) where
  type TemplateRep (AList k v) = Associative
  toTemplateValue = Associative . map (toTemplateValue *** toTemplateValue) . fromAList

instance (ToTemplateValue a, (TemplateRep a) ~ Single) => ToTemplateValue (V.Vector a) where
  type TemplateRep (V.Vector a) = List
  toTemplateValue = List . F.toList . fmap toTemplateValue

instance ToTemplateValue T.Text where
  toTemplateValue = Single . T.unpack

instance ToTemplateValue TL.Text where
  toTemplateValue = Single . TL.unpack

instance (ToTemplateValue a, TemplateRep a ~ Single) => ToTemplateValue (Maybe a) where
  toTemplateValue = maybe (Single "") toTemplateValue

instance (ToTemplateValue k, (TemplateRep k) ~ Single, ToTemplateValue v, (TemplateRep v) ~ Single) => ToTemplateValue (HS.HashMap k v) where
  type TemplateRep (HS.HashMap k v) = Associative
  toTemplateValue = toTemplateValue . AList . HS.toList

instance (ToTemplateValue k, (TemplateRep k) ~ Single, ToTemplateValue v, (TemplateRep v) ~ Single) => ToTemplateValue (MS.Map k v) where
  type TemplateRep (MS.Map k v) = Associative
  toTemplateValue = toTemplateValue . AList . MS.toList

instance ToTemplateValue a => ToTemplateValue (Identity a) where
  type TemplateRep (Identity a) = TemplateRep a
  toTemplateValue = toTemplateValue . runIdentity

instance
  ( ToTemplateValue a, TemplateRep a ~ Single
  , ToTemplateValue b, TemplateRep b ~ Single
  ) =>
  ToTemplateValue (a, b) where
  type TemplateRep (a, b) = List
  toTemplateValue (a, b) = List
    [ toTemplateValue a
    , toTemplateValue b
    ]

instance
  ( ToTemplateValue a, TemplateRep a ~ Single
  , ToTemplateValue b, TemplateRep b ~ Single
  , ToTemplateValue c, TemplateRep c ~ Single
  ) =>
  ToTemplateValue (a, b, c) where
  type TemplateRep (a, b, c) = List
  toTemplateValue (a, b, c) = List
    [ toTemplateValue a
    , toTemplateValue b
    , toTemplateValue c
    ]

instance
  ( ToTemplateValue a, TemplateRep a ~ Single
  , ToTemplateValue b, TemplateRep b ~ Single
  , ToTemplateValue c, TemplateRep c ~ Single
  , ToTemplateValue d, TemplateRep d ~ Single
  ) =>
  ToTemplateValue (a, b, c, d) where
  type TemplateRep (a, b, c, d) = List
  toTemplateValue (a, b, c, d) = List
    [ toTemplateValue a
    , toTemplateValue b
    , toTemplateValue c
    , toTemplateValue d
    ]

instance
  ( ToTemplateValue a, TemplateRep a ~ Single
  , ToTemplateValue b, TemplateRep b ~ Single
  , ToTemplateValue c, TemplateRep c ~ Single
  , ToTemplateValue d, TemplateRep d ~ Single
  , ToTemplateValue e, TemplateRep e ~ Single
  ) =>
  ToTemplateValue (a, b, c, d, e) where
  type TemplateRep (a, b, c, d, e) = List
  toTemplateValue (a, b, c, d, e) = List
    [ toTemplateValue a
    , toTemplateValue b
    , toTemplateValue c
    , toTemplateValue d
    , toTemplateValue e
    ]

instance
  ( ToTemplateValue a, TemplateRep a ~ Single
  , ToTemplateValue b, TemplateRep b ~ Single
  , ToTemplateValue c, TemplateRep c ~ Single
  , ToTemplateValue d, TemplateRep d ~ Single
  , ToTemplateValue e, TemplateRep e ~ Single
  , ToTemplateValue f, TemplateRep f ~ Single
  ) =>
  ToTemplateValue (a, b, c, d, e, f) where
  type TemplateRep (a, b, c, d, e, f) = List
  toTemplateValue (a, b, c, d, e, f) = List
    [ toTemplateValue a
    , toTemplateValue b
    , toTemplateValue c
    , toTemplateValue d
    , toTemplateValue e
    , toTemplateValue f
    ]

instance
  ( ToTemplateValue a, TemplateRep a ~ Single
  , ToTemplateValue b, TemplateRep b ~ Single
  , ToTemplateValue c, TemplateRep c ~ Single
  , ToTemplateValue d, TemplateRep d ~ Single
  , ToTemplateValue e, TemplateRep e ~ Single
  , ToTemplateValue f, TemplateRep f ~ Single
  , ToTemplateValue g, TemplateRep g ~ Single
  ) =>
  ToTemplateValue (a, b, c, d, e, f, g) where
  type TemplateRep (a, b, c, d, e, f, g) = List
  toTemplateValue (a, b, c, d, e, f, g) = List
    [ toTemplateValue a
    , toTemplateValue b
    , toTemplateValue c
    , toTemplateValue d
    , toTemplateValue e
    , toTemplateValue f
    , toTemplateValue g
    ]

data ValueModifier
  = Normal
  | Explode
  | MaxLength Int
  deriving (Read, Show, Eq)

data Variable = Variable
  { variableName :: String
  , variableValueModifier :: ValueModifier
  } deriving (Read, Show, Eq)

data TemplateSegment
  = Literal String -- ^ A literal string. No URI escaping will be performed
  | Embed Modifier [Variable] -- ^ An interpolation can have multiple variables (separated by commas in the textual format)
  deriving (Read, Show, Eq)

-- | A URI template is fundamentally a bunch of segments that are either constants
-- or else an interpolation
type UriTemplate = [TemplateSegment]

-- | How an interpolated value should be rendered
data Modifier
  = Simple -- ^ No prefix
  | Reserved -- ^ Prefixed by @+@
  | Fragment -- ^ Prefixed by @#@
  | Label -- ^ Prefixed by @.@
  | PathSegment -- ^ Prefixed by @/@
  | PathParameter -- ^ Prefixed by @;@
  | Query -- ^ Prefixed by @?@
  | QueryContinuation -- ^ Prefixed by @&@
  deriving (Read, Show, Eq)


