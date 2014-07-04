{-# LANGUAGE EmptyDataDecls, GADTs, FunctionalDependencies, MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances, FlexibleInstances, TypeFamilies, OverlappingInstances, GeneralizedNewtypeDeriving #-}
module Network.URI.Template.Types where
import Control.Arrow
import Data.Foldable as F
import Data.List
import qualified Data.String as S
import qualified Data.HashMap.Strict as HS
import qualified Data.Map.Strict as MS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V

data Single
data Associative
data List

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

newtype TemplateString = String { fromString :: String }
  deriving (Read, Show, Eq, S.IsString)

newtype AList k v = AList { fromAList :: [(k, v)] }

class ToTemplateValue a where
  type TemplateRep a
  toTemplateValue :: a -> TemplateValue (TemplateRep a)

instance ToTemplateValue Int where
  type TemplateRep Int = Single
  toTemplateValue = Single . show

instance ToTemplateValue TemplateString where
  type TemplateRep TemplateString = Single
  toTemplateValue = Single . fromString

instance (ToTemplateValue a, (TemplateRep a) ~ Single) => ToTemplateValue [a] where
  type TemplateRep [a] = List
  toTemplateValue = List . map toTemplateValue

instance (ToTemplateValue k, (TemplateRep k) ~ Single, ToTemplateValue v, (TemplateRep v) ~ Single) => ToTemplateValue (AList k v) where
  type TemplateRep (AList k v) = Associative
  toTemplateValue = Associative . map (toTemplateValue *** toTemplateValue) . fromAList

instance (ToTemplateValue a, (TemplateRep a) ~ Single) => ToTemplateValue (V.Vector a) where
  type TemplateRep (V.Vector a) = List
  toTemplateValue = List . F.toList . fmap toTemplateValue

instance ToTemplateValue T.Text where
  type TemplateRep T.Text = Single
  toTemplateValue = Single . T.unpack

instance ToTemplateValue TL.Text where
  type TemplateRep TL.Text = Single
  toTemplateValue = Single . TL.unpack

instance (ToTemplateValue k, (TemplateRep k) ~ Single, ToTemplateValue v, (TemplateRep v) ~ Single) => ToTemplateValue (HS.HashMap k v) where
  type TemplateRep (HS.HashMap k v) = Associative
  toTemplateValue = toTemplateValue . AList . HS.toList

instance (ToTemplateValue k, (TemplateRep k) ~ Single, ToTemplateValue v, (TemplateRep v) ~ Single) => ToTemplateValue (MS.Map k v) where
  type TemplateRep (MS.Map k v) = Associative
  toTemplateValue = toTemplateValue . AList . MS.toList

data ValueModifier
  = Normal
  | Explode
  | MaxLength Int
  deriving (Read, Show, Eq)

data Variable = Variable { variableName :: String, variableValueModifier :: ValueModifier }
  deriving (Read, Show, Eq)

data TemplateSegment
  = Literal String
  | Embed Modifier [Variable]
  deriving (Read, Show, Eq)

type UriTemplate = [TemplateSegment]

data Modifier
  = Simple
  | Reserved
  | Fragment
  | Label
  | PathSegment
  | PathParameter
  | Query
  | QueryContinuation
  | Alias
  deriving (Read, Show, Eq)


