{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.URI.Template.Internal where
import Control.Monad.Writer.Strict
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB

import Data.DList hiding (map)
import Data.List (intersperse)
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Network.HTTP.Base (urlEncode)
import Network.URI.Template.Types

class Monoid (StringBuilder a) => Buildable a where
  type StringBuilder a
  build :: StringBuilder a -> a
  addChar :: Proxy a -> Char -> StringBuilder a
  addString :: Proxy a -> String -> StringBuilder a

instance Buildable String where
  type StringBuilder String = DList Char
  build = Data.DList.toList
  addChar _ = singleton
  addString _ = fromList

instance Buildable BS.ByteString where
  type StringBuilder BS.ByteString = BB.Builder
  build = BL.toStrict . BB.toLazyByteString
  addChar _ = BB.char8
  addString _ = BB.string8

instance Buildable BL.ByteString where
  type StringBuilder BL.ByteString = BB.Builder
  build = BB.toLazyByteString
  addChar _ = BB.char8
  addString _ = BB.string8

instance Buildable T.Text where
  type StringBuilder T.Text = TB.Builder
  build = TL.toStrict . TB.toLazyText
  addChar _ = TB.singleton
  addString _ = TB.fromString

instance Buildable TL.Text where
  type StringBuilder TL.Text = TB.Builder
  build = TB.toLazyText
  addChar _ = TB.singleton
  addString _ = TB.fromString

instance Buildable BB.Builder where
  type StringBuilder BB.Builder = BB.Builder
  build = id
  addChar _ = BB.char8
  addString _ = BB.string8

instance Buildable TB.Builder where
  type StringBuilder TB.Builder = TB.Builder
  build = id
  addChar _ = TB.singleton
  addString _ = TB.fromString

-- instance Buildable (Path, QueryParams, Hash)

data Allow = Unreserved | UnreservedOrReserved

allowEncoder Unreserved           = urlEncode
allowEncoder UnreservedOrReserved = id

data ProcessingOptions = ProcessingOptions
  { modifierPrefix :: Maybe Char
  , modifierSeparator :: Char
  , modifierSupportsNamed :: Bool
  , modifierIfEmpty :: Maybe Char
  , modifierAllow :: Allow
  }

type BoundValue = (String, WrappedValue)

option :: Maybe Char -> Char -> Bool -> Maybe Char -> Allow -> ProcessingOptions
option = ProcessingOptions

options :: Modifier -> ProcessingOptions
options m = case m of
  Simple            -> option  Nothing   ',' False  Nothing    Unreserved
  Reserved          -> option  Nothing   ',' False  Nothing    UnreservedOrReserved
  Label             -> option (Just '.') '.' False  Nothing    Unreserved
  PathSegment       -> option (Just '/') '/' False  Nothing    Unreserved
  PathParameter     -> option (Just ';') ';' True   Nothing    Unreserved
  Query             -> option (Just '?') '&' True  (Just '=')  Unreserved
  QueryContinuation -> option (Just '&') '&' True  (Just '=')  Unreserved
  Fragment          -> option (Just '#') ',' False  Nothing    UnreservedOrReserved

templateValueIsEmpty :: TemplateValue a -> Bool
templateValueIsEmpty (Single s)      = null s
templateValueIsEmpty (Associative s) = null s
templateValueIsEmpty (List s)        = null s

namePrefix :: forall str a.
              (Buildable str)
           => Proxy str -> ProcessingOptions -> String -> TemplateValue a -> StringBuilder str
namePrefix p opts name val = addString p name <> if templateValueIsEmpty val
  then maybe mempty (addChar p) $ modifierIfEmpty opts
  else addChar p '='

whenM :: Monoid m => Bool -> m -> m
whenM pred m = if pred then m else mempty

processVariable
  :: forall str.
     (Buildable str)
  => Proxy str -> Modifier -> Bool -> Variable -> WrappedValue -> StringBuilder str
processVariable p m isFirst (Variable varName varMod) (WrappedValue val) =
  let prefix = maybe mempty (addChar p) $ modifierPrefix settings
      separator = addChar p $ modifierSeparator settings
      rest = case varMod of
        Normal -> do
          whenM
            (modifierSupportsNamed settings)
            (namePrefix p settings varName val) <> unexploded
        Explode -> exploded
        MaxLength l ->
          whenM
            (modifierSupportsNamed settings)
            (namePrefix p settings varName val) <> unexploded
  in (if isFirst then prefix else separator) <> rest
  where
    settings = options m

    addEncodeString :: String -> StringBuilder str
    addEncodeString = addString p . (allowEncoder $ modifierAllow settings)

    sepByCommas = mconcat . intersperse (addChar p ',')

    associativeCommas :: (String -> String) -> (TemplateValue Single, TemplateValue Single) -> StringBuilder str
    associativeCommas f (Single n, Single v) =
      addEncodeString n <>
      addChar p ',' <>
      addEncodeString (f v)

    preprocess :: String -> String
    preprocess = case varMod of
      MaxLength l -> take l
      _ -> id

    unexploded = case val of
      (Associative l) -> sepByCommas $ map (associativeCommas preprocess) l
      (List l) -> sepByCommas $ map (\(Single s) -> addEncodeString $ preprocess s) l
      (Single s) -> addEncodeString $ preprocess s

    explodedAssociative :: (TemplateValue Single, TemplateValue Single) -> StringBuilder str
    explodedAssociative (Single k, Single v) =
      addEncodeString k <>
      addChar p '=' <>
      addEncodeString (preprocess v)

    exploded :: StringBuilder str
    exploded = case val of
      (Single s) -> whenM
                    (modifierSupportsNamed settings)
                    (namePrefix p settings varName val) <> addEncodeString (preprocess s)
      (Associative l) ->
        mconcat $
        intersperse (addChar p $ modifierSeparator settings) $
        map explodedAssociative l
      (List l) ->
        mconcat $
        intersperse (addChar p $ modifierSeparator settings) $
        map (\(Single s) ->
               whenM
                 (modifierSupportsNamed settings)
                 (namePrefix p settings varName val) <> addEncodeString (preprocess s)) l

processVariables :: forall str.
                    (Buildable str)
                 => Proxy str -> [(String, WrappedValue)] -> Modifier -> [Variable] -> StringBuilder str
processVariables p env m vs = mconcat processedVariables
  where
    findValue (Variable varName _) = lookup varName env

    nonEmptyVariables :: [(Variable, WrappedValue)]
    nonEmptyVariables = catMaybes $ map (\v -> fmap (\mv -> (v, mv)) $ findValue v) vs

    processors :: [Variable -> WrappedValue -> StringBuilder str]
    processors = (processVariable p m True) : repeat (processVariable p m False)

    processedVariables :: [StringBuilder str]
    processedVariables = zipWith uncurry processors nonEmptyVariables

render :: (Buildable str) => UriTemplate -> [BoundValue] -> str
render tpl env = render' tpl env

render' :: forall str. (Buildable str) => UriTemplate -> [BoundValue] -> str
render' tpl env = build $ mconcat $ map go tpl
  where
    p :: Proxy str
    p = Proxy

    go :: TemplateSegment -> StringBuilder str
    go (Literal s) = addString p s
    go (Embed m vs) = processVariables p env m vs



