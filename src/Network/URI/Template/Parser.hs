{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Network.URI.Template.Parser (
  parseTemplate,
) where

import qualified Data.ByteString as BS
import qualified Data.Char as C
import qualified Data.String as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import FlatParse.Basic
import Network.URI.Template.Error
import Network.URI.Template.Types


-- | Parse a character in a specific range
{-# INLINE charRange #-}
charRange :: Char -> Char -> Parser e Char
charRange l r = satisfy (\c -> l <= c && c <= r)


-- | Parse a character matching any of the given ranges
-- Uses fusedSatisfy for better performance when possible
charRanges :: [(Char, Char)] -> Parser e Char
charRanges [] = empty
charRanges ((l, r) : rest) = satisfy (\c -> l <= c && c <= r) <|> charRanges rest


-- | Parse Unicode characters as specified in RFC 6570
ucschar :: Parser e Char
ucschar =
  charRanges
    [ ('\xA0', '\xD7FF')
    , ('\xF900', '\xFDCF')
    , ('\xFDF0', '\xFFEF')
    , ('\x10000', '\x1FFFD')
    , ('\x20000', '\x2FFFD')
    , ('\x30000', '\x3FFFD')
    , ('\x40000', '\x4FFFD')
    , ('\x50000', '\x5FFFD')
    , ('\x60000', '\x6FFFD')
    , ('\x70000', '\x7FFFD')
    , ('\x80000', '\x8FFFD')
    , ('\x90000', '\x9FFFD')
    , ('\xA0000', '\xAFFFD')
    , ('\xB0000', '\xBFFFD')
    , ('\xC0000', '\xCFFFD')
    , ('\xD0000', '\xDFFFD')
    , ('\xE1000', '\xEFFFD')
    ]


-- | Parse private use area characters
iprivate :: Parser e Char
iprivate =
  charRanges
    [ ('\xE000', '\xF8FF')
    , ('\xF0000', '\xFFFFD')
    , ('\x100000', '\x10FFFD')
    ]


-- | Parse a percent-encoded sequence
{-# INLINE pctEncoded #-}
pctEncoded :: Parser e String
pctEncoded = do
  $(char '%')
  d1 <- hexDigit
  d2 <- hexDigit
  return ['%', d1, d2]
 where
  {-# INLINE hexDigit #-}
  hexDigit = satisfy (\c -> C.isDigit c || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'))


-- | Parse a literal character
literalChar :: Parser e Char
literalChar =
  ($(char '\x21') >> pure '\x21')
    <|> ($(char '\x23') >> pure '\x23')
    <|> ($(char '\x24') >> pure '\x24')
    <|> ($(char '\x26') >> pure '\x26')
    <|> ($(char '\x3D') >> pure '\x3D')
    <|> ($(char '\x5D') >> pure '\x5D')
    <|> ($(char '\x5F') >> pure '\x5F')
    <|> ($(char '\x7E') >> pure '\x7E')
    <|> charRanges [('\x28', '\x3B'), ('\x3F', '\x5B'), ('\x61', '\x7A')]
    <|> ucschar
    <|> iprivate


-- | Parse a literal segment
-- Uses byteStringOf to avoid intermediate list allocations
{-# INLINE literal #-}
literal :: Parser e TemplateSegment
literal = Literal . TE.decodeUtf8 <$> byteStringOf (skipSome literalCharOrPct)
 where
  {-# INLINE literalCharOrPct #-}
  literalCharOrPct = (() <$ literalChar) <|> (() <$ pctEncoded)


-- | Parse variables in an embed
{-# INLINE variables #-}
variables :: Parser e TemplateSegment
variables = do
  mod <- modifier
  vars <- sepBy1 variable (ws *> $(char ',') *> ws)
  return $ Embed mod vars


-- | Parse optional whitespace
{-# INLINE ws #-}
ws :: Parser e ()
ws = skipMany (satisfy (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r'))


-- | Parse a modifier character
modifier :: Parser e Modifier
modifier =
  ($(char '+') *> pure Reserved)
    <|> ($(char '#') *> pure Fragment)
    <|> ($(char '.') *> pure Label)
    <|> ($(char '/') *> pure PathSegment)
    <|> ($(char ';') *> pure PathParameter)
    <|> ($(char '?') *> pure Query)
    <|> ($(char '&') *> pure QueryContinuation)
    <|> ($(char '=') *> pure Reserved)
    <|> ($(char '@') *> pure Reserved)
    <|> ($(char '!') *> pure Reserved)
    <|> ($(char '|') *> pure Reserved)
    <|> pure Simple


-- | Parse a variable
variable :: Parser e Variable
variable = do
  nm <- name
  valMod <- valueModifier
  return $ Variable nm valMod
 where
  {-# INLINE name #-}
  name = TE.decodeUtf8 <$> byteStringOf (skipSome nameChar)
   where
    {-# INLINE nameChar #-}
    nameChar = (() <$ alphaNum) <|> (() <$ $(char '_')) <|> (() <$ pctEncoded)
  {-# INLINE valueModifier #-}
  valueModifier =
    ($(char '*') *> pure Explode)
      <|> ($(char ':') *> (MaxLength <$> parseInt))
      <|> pure Normal
  {-# INLINE parseInt #-}
  parseInt = do
    digits <- some digit
    return $ read digits
  {-# INLINE alphaNum #-}
  alphaNum = satisfy (\c -> C.isAlphaNum c)
  {-# INLINE digit #-}
  digit = satisfy C.isDigit


-- | Parse an embedded variable expression
{-# INLINE embed #-}
embed :: Parser e TemplateSegment
embed = $(char '{') *> variables <* $(char '}')


-- | Parse a URI template segments
{-# INLINE uriTemplate #-}
uriTemplate :: Parser e (V.Vector TemplateSegment)
uriTemplate = do
  ws
  segments <- many (literal <|> embed)
  eof  -- Ensure we consumed all input
  return (V.fromList segments)


-- | Helper to separate a parser by a separator
{-# INLINE sepBy1 #-}
sepBy1 :: Parser e a -> Parser e sep -> Parser e [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)


-- | Analyze parse failure to provide a better error message
analyzeParseFailure :: String -> ParseError
analyzeParseFailure input = go 0 0 input
 where
  go depth pos []
    | depth > 0 = UnterminatedExpression (findLastOpen input)
    | otherwise = GenericParseError ("Parse error in URI template: " ++ input)
  go depth pos ('{' : rest) = go (depth + 1) (pos + 1) rest
  go depth pos ('}' : rest)
    | depth <= 0 = UnexpectedCharacter pos '}' "Unexpected closing brace without matching opening brace"
    | otherwise = go (depth - 1) (pos + 1) rest
  go depth pos (_ : rest) = go depth (pos + 1) rest

  findLastOpen = go' 0 0
   where
    go' pos lastPos [] = lastPos
    go' pos lastPos ('{' : rest) = go' (pos + 1) pos rest
    go' pos lastPos (_ : rest) = go' (pos + 1) lastPos rest


-- | Parse a template from a String
parseTemplate :: String -> Either ParseError UriTemplate
parseTemplate input =
  case runParser uriTemplate (TE.encodeUtf8 $ T.pack input) of
    OK result _ -> Right (UriTemplate result)
    Err _ -> Left (analyzeParseFailure input)
    Fail -> Left (analyzeParseFailure input)


{- | 'IsString' instance for 'UriTemplate' allows using string literals directly as templates
when @OverloadedStrings@ is enabled.

>>> :set -XOverloadedStrings
>>> let template = "/users/{userId}" :: UriTemplate
>>> renderTemplate template
"/users/{userId}"

Note: If the string fails to parse as a valid URI template, this will call 'error'.
For fallible parsing, use 'parseTemplate' instead.
-}
instance S.IsString UriTemplate where
  fromString s = case parseTemplate s of
    Right template -> template
    Left err -> error $ "Invalid URI template: " ++ s ++ "\n" ++ show err
