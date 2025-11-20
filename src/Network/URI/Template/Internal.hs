{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Network.URI.Template.Internal where

import Control.Monad.Writer.Strict
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.DList hiding (map)
import Data.List (intersperse)
import Data.Maybe
import Data.Monoid
import Data.Proxy
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Encoding as TL
import Network.HTTP.Types.URI
import Network.URI.Template.Types
import Web.HttpApiData (toUrlPiece)
import Web.Internal.HttpApiData (ToHttpApiData (..))


class Monoid (Builder a) => Buildable a where
  type Builder a


  -- | Convert the intermediate output into the end result
  build :: Builder a -> a


  -- | Construct an appendable character representation
  addChar :: Proxy a -> Char -> Builder a


  -- | Construct an appendable string representation
  addString :: Proxy a -> T.Text -> Builder a


  -- | Convert a bytestring builder into the appropriate format
  addBytestringBuilder :: Proxy a -> BB.Builder -> Builder a


instance Buildable String where
  type Builder String = DList Char
  build = Data.DList.toList
  addChar _ = singleton
  addString _ = fromList . T.unpack
  addBytestringBuilder _ = fromList . BL.unpack . BB.toLazyByteString


instance Buildable BS.ByteString where
  type Builder BS.ByteString = BB.Builder
  build = BL.toStrict . BB.toLazyByteString
  addChar _ = BB.char8
  addString _ = BB.byteString . T.encodeUtf8
  addBytestringBuilder _ = id


instance Buildable BL.ByteString where
  type Builder BL.ByteString = BB.Builder
  build = BB.toLazyByteString
  addChar _ = BB.char8
  addString _ = BB.byteString . T.encodeUtf8
  addBytestringBuilder _ = id


instance Buildable T.Text where
  type Builder T.Text = TB.Builder
  build = TL.toStrict . TB.toLazyText
  addChar _ = TB.singleton
  addString _ = TB.fromText
  addBytestringBuilder _ = TB.fromLazyText . TL.decodeUtf8 . BB.toLazyByteString


instance Buildable TL.Text where
  type Builder TL.Text = TB.Builder
  build = TB.toLazyText
  addChar _ = TB.singleton
  addString _ = TB.fromText
  addBytestringBuilder _ = TB.fromLazyText . TL.decodeUtf8 . BB.toLazyByteString


instance Buildable BB.Builder where
  type Builder BB.Builder = BB.Builder
  build = id
  addChar _ = BB.char8
  addString _ = BB.byteString . T.encodeUtf8
  addBytestringBuilder _ = id


instance Buildable TB.Builder where
  type Builder TB.Builder = TB.Builder
  build = id
  addChar _ = TB.singleton
  addString _ = TB.fromText
  addBytestringBuilder _ = TB.fromLazyText . TL.decodeUtf8 . BB.toLazyByteString


-- instance Buildable (Path, QueryParams, Hash)

data Allow = Unreserved | UnreservedOrReserved


allowEncoder :: (Buildable str, ToHttpApiData a) => Proxy str -> Allow -> a -> Builder str
allowEncoder p Unreserved = addBytestringBuilder p . urlEncodeBuilder True . T.encodeUtf8 . toUrlPiece
allowEncoder p UnreservedOrReserved = addString p . toUrlPiece


data ProcessingOptions = ProcessingOptions
  { modifierPrefix :: Maybe Char
  , modifierSeparator :: Char
  , modifierSupportsNamed :: Bool
  , modifierIfEmpty :: Maybe Char
  , modifierAllow :: Allow
  }


type BoundValue = (T.Text, WrappedValue)


option :: Maybe Char -> Char -> Bool -> Maybe Char -> Allow -> ProcessingOptions
option = ProcessingOptions


options :: Modifier -> ProcessingOptions
options m = case m of
  Simple -> option Nothing ',' False Nothing Unreserved
  Reserved -> option Nothing ',' False Nothing UnreservedOrReserved
  Label -> option (Just '.') '.' False Nothing Unreserved
  PathSegment -> option (Just '/') '/' False Nothing Unreserved
  PathParameter -> option (Just ';') ';' True Nothing Unreserved
  Query -> option (Just '?') '&' True (Just '=') Unreserved
  QueryContinuation -> option (Just '&') '&' True (Just '=') Unreserved
  Fragment -> option (Just '#') ',' False Nothing UnreservedOrReserved


templateValueIsEmpty :: TemplateValue a -> Bool
templateValueIsEmpty (Single s) = T.null $ toUrlPiece s
templateValueIsEmpty (Associative s) = null s
templateValueIsEmpty (List s) = null s
{-# INLINE templateValueIsEmpty #-}


namePrefix ::
  forall str a.
  (Buildable str) =>
  Proxy str ->
  ProcessingOptions ->
  T.Text ->
  TemplateValue a ->
  Builder str
namePrefix p opts name val =
  addString p name
    <> if templateValueIsEmpty val
      then maybe mempty (addChar p) $ modifierIfEmpty opts
      else addChar p '='


whenM :: Monoid m => Bool -> m -> m
whenM pred m = if pred then m else mempty


processVariable ::
  forall str.
  (Buildable str) =>
  Proxy str ->
  Modifier ->
  Bool ->
  Variable ->
  WrappedValue ->
  Builder str
processVariable p m isFirst (Variable varName varMod) (WrappedValue val) =
  let prefix = maybe mempty (addChar p) $ modifierPrefix settings
      separator = addChar p $ modifierSeparator settings
      rest = case varMod of
        Normal -> do
          whenM
            (modifierSupportsNamed settings)
            (namePrefix p settings varName val)
            <> unexploded
        Explode -> exploded
        MaxLength l ->
          whenM
            (modifierSupportsNamed settings)
            (namePrefix p settings varName val)
            <> unexploded
   in (if isFirst then prefix else separator) <> rest
 where
  settings = options m

  addEncodeString :: ToHttpApiData a => a -> Builder str
  addEncodeString = allowEncoder (Proxy @str) (modifierAllow settings)

  sepByCommas = mconcat . intersperse (addChar p ',')

  associativeCommas :: (TemplateValue Single, TemplateValue Single) -> Builder str
  associativeCommas (Single n, Single v) =
    addEncodeString n
      <> addChar p ','
      <> addEncodeString (preprocess v)

  preprocess :: ToHttpApiData a => a -> T.Text
  preprocess = case varMod of
    MaxLength l -> T.take l . toUrlPiece
    _ -> toUrlPiece

  unexploded :: Builder str
  unexploded = case val of
    (Associative l) -> sepByCommas $ map associativeCommas l
    (List l) -> sepByCommas $ map (\(Single s) -> addEncodeString $ preprocess s) l
    (Single s) -> addEncodeString $ preprocess s

  explodedAssociative :: (TemplateValue Single, TemplateValue Single) -> Builder str
  explodedAssociative (Single k, Single v) =
    addEncodeString k
      <> addChar p '='
      <> addEncodeString (preprocess v)

  exploded :: Builder str
  exploded = case val of
    (Single s) ->
      whenM
        (modifierSupportsNamed settings)
        (namePrefix p settings varName val)
        <> addEncodeString (preprocess s)
    (Associative l) ->
      mconcat $
        intersperse (addChar p $ modifierSeparator settings) $
          map explodedAssociative l
    (List l) ->
      mconcat $
        intersperse (addChar p $ modifierSeparator settings) $
          map
            ( \(Single s) ->
                whenM
                  (modifierSupportsNamed settings)
                  (namePrefix p settings varName val)
                  <> addEncodeString (preprocess s)
            )
            l


processVariables ::
  forall str.
  (Buildable str) =>
  Proxy str ->
  [(T.Text, WrappedValue)] ->
  Modifier ->
  [Variable] ->
  Builder str
processVariables p env m vs = mconcat processedVariables
 where
  findValue (Variable varName _) = lookup varName env

  nonEmptyVariables :: [(Variable, WrappedValue)]
  nonEmptyVariables = mapMaybe (\v -> (\mv -> (v, mv)) <$> findValue v) vs

  processors :: [Variable -> WrappedValue -> Builder str]
  processors = processVariable p m True : repeat (processVariable p m False)

  processedVariables :: [Builder str]
  processedVariables = zipWith uncurry processors nonEmptyVariables


render :: (Buildable str) => UriTemplate -> [BoundValue] -> str
render = render'


render' :: forall str. (Buildable str) => UriTemplate -> [BoundValue] -> str
render' (UriTemplate tpl) env = build $ mconcat $ map go tpl
 where
  p :: Proxy str
  p = Proxy

  go :: TemplateSegment -> Builder str
  go (Literal s) = addString p s
  go (Embed m vs) = processVariables p env m vs
