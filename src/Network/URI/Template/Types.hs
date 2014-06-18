{-# LANGUAGE EmptyDataDecls, GADTs, FunctionalDependencies, MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
module Network.URI.Template.Types where

data SingleElement
data AssociativeListElement
data ListElement

newtype ListElem a = ListElem { fromListElem :: a }

data TemplateValue a where
  Single :: String -> TemplateValue SingleElement
  Associative :: [(String, TemplateValue SingleElement)] -> TemplateValue AssociativeListElement
  List :: [TemplateValue SingleElement] -> TemplateValue ListElement

data InternalTemplateValue
  = SingleVal String
  | AssociativeVal [(String, String)]
  | ListVal [String]

fromSingle :: TemplateValue SingleElement -> String
fromSingle (Single s) = s

fromSingleVal :: InternalTemplateValue -> String
fromSingleVal (SingleVal s) = s

internalize :: TemplateValue a -> InternalTemplateValue
internalize (Single s) = SingleVal s
internalize (Associative ls) = AssociativeVal $ map (\(l, r) -> (l, fromSingleVal $ internalize r)) ls
internalize (List l) = ListVal $ map (fromSingleVal . internalize) l

class ToTemplateValue a e | a -> e where
  toTemplateValue :: a -> TemplateValue e

instance ToTemplateValue Int SingleElement where
  toTemplateValue = Single . show

instance ToTemplateValue a SingleElement => ToTemplateValue (ListElem [a]) ListElement where
  toTemplateValue = List . map toTemplateValue . fromListElem

instance ToTemplateValue a SingleElement => ToTemplateValue [(String, a)] AssociativeListElement where
  toTemplateValue = Associative . map (\(l, r) -> (l, toTemplateValue r))

instance ToTemplateValue String SingleElement where
  toTemplateValue = Single

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


