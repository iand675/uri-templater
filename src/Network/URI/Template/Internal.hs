{-# LANGUAGE RankNTypes, GADTs, ScopedTypeVariables #-}
module Network.URI.Template.Internal where
import Control.Monad.Writer.Strict
import Data.DList hiding (map)
import Data.List (intersperse)
import Data.Maybe
import Data.Monoid
import Network.HTTP.Base (urlEncode)
import Network.URI.Template.Types

type StringBuilder = Writer (DList Char)

addChar :: Char -> StringBuilder ()
addChar = tell . singleton

addString :: String -> StringBuilder ()
addString = tell . fromList

data Allow = Unreserved | UnreservedOrReserved

allowEncoder Unreserved = urlEncode
allowEncoder UnreservedOrReserved = id

data ProcessingOptions = ProcessingOptions
  { modifierPrefix :: Maybe Char
  , modifierSeparator :: Char
  , modifierSupportsNamed :: Bool
  , modifierIfEmpty :: Maybe Char
  , modifierAllow :: Allow
  }

option :: Maybe Char -> Char -> Bool -> Maybe Char -> Allow -> ProcessingOptions
option = ProcessingOptions

options :: Modifier -> ProcessingOptions
options m = case m of
  Simple            -> option Nothing    ',' False Nothing    Unreserved
  Reserved          -> option Nothing    ',' False Nothing    UnreservedOrReserved
  Label             -> option (Just '.') '.' False Nothing    Unreserved
  PathSegment       -> option (Just '/') '/' False Nothing    Unreserved
  PathParameter     -> option (Just ';') ';' True  Nothing    Unreserved
  Query             -> option (Just '?') '&' True  (Just '=') Unreserved
  QueryContinuation -> option (Just '&') '&' True  (Just '=') Unreserved
  Fragment          -> option (Just '#') ',' False Nothing    UnreservedOrReserved

templateValueIsEmpty :: InternalTemplateValue -> Bool
templateValueIsEmpty (SingleVal s) = null s
templateValueIsEmpty (AssociativeVal s) = null s
templateValueIsEmpty (ListVal s) = null s

namePrefix :: ProcessingOptions -> String -> InternalTemplateValue -> StringBuilder ()
namePrefix opts name val = do
  addString name
  if templateValueIsEmpty val
    then maybe (return ()) addChar $ modifierIfEmpty opts
    else addChar '='

processVariable :: Modifier -> Bool -> Variable -> InternalTemplateValue -> StringBuilder ()
processVariable m isFirst (Variable varName varMod) val = do
  if isFirst
    then maybe (return ()) addChar $ modifierPrefix settings
    else addChar $ modifierSeparator settings
  case varMod of
    Normal -> do
      when (modifierSupportsNamed settings) (namePrefix settings varName val)
      unexploded
    Explode -> exploded
    (MaxLength l) -> do
      when (modifierSupportsNamed settings) (namePrefix settings varName val)
      -- TODO: this is wrong. we need to truncate prior to encoding.
      censor (fromList . take l . toList) unexploded
  where
    settings = options m
    addEncodeString = addString . (allowEncoder $ modifierAllow settings)
    sepByCommas = sequence_ . intersperse (addChar ',')
    associativeCommas (n, v) = addEncodeString n >> addChar ',' >> addEncodeString v
    unexploded = case val of
      (AssociativeVal l) -> sepByCommas $ map associativeCommas l
      (ListVal l) -> sepByCommas $ map addEncodeString l
      (SingleVal s) -> addEncodeString s
    explodedAssociative (k, v) = do
      addEncodeString k
      addChar '='
      addEncodeString v
    exploded :: StringBuilder ()
    exploded = case val of
      (SingleVal s) -> do
        when (modifierSupportsNamed settings) (namePrefix settings varName val)
        addEncodeString s
      (AssociativeVal l) -> sequence_ $ intersperse (addChar $ modifierSeparator settings) $ map explodedAssociative l
      (ListVal l) -> sequence_ $ intersperse (addChar $ modifierSeparator settings) $ map addEncodeString l

processVariables :: [(String, InternalTemplateValue)] -> Modifier -> [Variable] -> StringBuilder ()
processVariables env m vs = sequence_ $ processedVariables
  where
    findValue (Variable varName _) = lookup varName env
    nonEmptyVariables :: [(Variable, InternalTemplateValue)]
    nonEmptyVariables = catMaybes $ map (\v -> fmap (\mv -> (v, mv)) $ findValue v) vs
    processors :: [Variable -> InternalTemplateValue -> StringBuilder ()]
    processors = (processVariable m True) : repeat (processVariable m False)
    processedVariables :: [StringBuilder ()]
    processedVariables = zipWith uncurry processors nonEmptyVariables

render :: forall a. UriTemplate -> [(String, TemplateValue a)] -> String
render tpl env = render' tpl $ map (\(l, r) -> (l, internalize r)) env

render' :: UriTemplate -> [(String, InternalTemplateValue)] -> String
render' tpl env = toList $ execWriter $ mapM_ go tpl
  where
    go :: TemplateSegment -> StringBuilder ()
    go (Literal s) = addString s
    go (Embed m vs) = processVariables env m vs
      {-(processVariable m True) : repeat (processVariable m False)-}
