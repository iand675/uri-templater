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

namePrefix :: ProcessingOptions -> String -> TemplateValue a -> StringBuilder ()
namePrefix opts name val = do
  addString name
  if templateValueIsEmpty val
    then maybe (return ()) addChar $ modifierIfEmpty opts
    else addChar '='

processVariable :: Modifier -> Bool -> Variable -> WrappedValue -> StringBuilder ()
processVariable m isFirst (Variable varName varMod) (WrappedValue val) = do
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
      unexploded
  where
    settings = options m

    addEncodeString :: String -> StringBuilder ()
    addEncodeString = addString . (allowEncoder $ modifierAllow settings)

    sepByCommas = sequence_ . intersperse (addChar ',')

    associativeCommas :: (String -> String) -> (TemplateValue Single, TemplateValue Single) -> StringBuilder ()
    associativeCommas f (Single n, Single v) = addEncodeString n >> addChar ',' >> addEncodeString (f v)

    preprocess :: String -> String
    preprocess = case varMod of
      MaxLength l -> take l
      _ -> id

    unexploded = case val of
      (Associative l) -> sepByCommas $ map (associativeCommas preprocess) l
      (List l) -> sepByCommas $ map (\(Single s) -> addEncodeString $ preprocess s) l
      (Single s) -> addEncodeString $ preprocess s

    explodedAssociative :: (TemplateValue Single, TemplateValue Single) -> StringBuilder ()
    explodedAssociative (Single k, Single v) = do
      addEncodeString k
      addChar '='
      addEncodeString $ preprocess v

    exploded :: StringBuilder ()
    exploded = case val of
      (Single s) -> do
        when (modifierSupportsNamed settings) (namePrefix settings varName val)
        addEncodeString $ preprocess s
      (Associative l) -> sequence_ $ intersperse (addChar $ modifierSeparator settings) $ map explodedAssociative l
      (List l) -> sequence_ $ intersperse (addChar $ modifierSeparator settings) $ map (\(Single s) -> addEncodeString $ preprocess s) l

processVariables :: [(String, WrappedValue)] -> Modifier -> [Variable] -> StringBuilder ()
processVariables env m vs = sequence_ $ processedVariables
  where
    findValue (Variable varName _) = lookup varName env

    nonEmptyVariables :: [(Variable, WrappedValue)]
    nonEmptyVariables = catMaybes $ map (\v -> fmap (\mv -> (v, mv)) $ findValue v) vs

    processors :: [Variable -> WrappedValue -> StringBuilder ()]
    processors = (processVariable m True) : repeat (processVariable m False)

    processedVariables :: [StringBuilder ()]
    processedVariables = zipWith uncurry processors nonEmptyVariables

render :: UriTemplate -> [BoundValue] -> String
render tpl env = render' tpl env

render' :: UriTemplate -> [BoundValue] -> String
render' tpl env = Data.DList.toList $ execWriter $ mapM_ go tpl
  where
    go :: TemplateSegment -> StringBuilder ()
    go (Literal s) = addString s
    go (Embed m vs) = processVariables env m vs



