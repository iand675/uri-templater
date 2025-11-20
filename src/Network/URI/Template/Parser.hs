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
import Data.Text.Prettyprint.Doc (Doc, pretty)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import FlatParse.Basic
import Network.URI.Template.Types


-- | Parse a character in a specific range
charRange :: Char -> Char -> Parser e Char
charRange l r = satisfy (\c -> l <= c && c <= r)


-- | Parse a character matching any of the given ranges
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
pctEncoded :: Parser e String
pctEncoded = do
  $(char '%')
  d1 <- hexDigit
  d2 <- hexDigit
  return ['%', d1, d2]
 where
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
literal :: Parser e TemplateSegment
literal = do
  chunks <- some ((pure <$> literalChar) <|> pctEncoded)
  return $ Literal (T.pack $ concat chunks)


-- | Parse variables in an embed
variables :: Parser e TemplateSegment
variables = do
  mod <- modifier
  vars <- sepBy1 variable (ws *> $(char ',') *> ws)
  return $ Embed mod vars


-- | Parse optional whitespace
ws :: Parser e ()
ws = skipMany ($(char ' ') <|> $(char '\t') <|> $(char '\n') <|> $(char '\r'))


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
  name = do
    chunks <- some ((pure <$> alphaNum) <|> ($(char '_') >> pure "_") <|> pctEncoded)
    return $ T.pack $ concat chunks
  valueModifier =
    ($(char '*') *> pure Explode)
      <|> ($(char ':') *> (MaxLength <$> parseInt))
      <|> pure Normal
  parseInt = do
    digits <- some digit
    return $ read digits
  alphaNum = satisfy (\c -> C.isAlphaNum c)
  digit = satisfy C.isDigit


-- | Parse an embedded variable expression
embed :: Parser e TemplateSegment
embed = $(char '{') *> variables <* $(char '}')


-- | Parse a URI template segments
uriTemplate :: Parser e [TemplateSegment]
uriTemplate = ws *> many (literal <|> embed)


-- | Helper to separate a parser by a separator
sepBy1 :: Parser e a -> Parser e sep -> Parser e [a]
sepBy1 p sep = do
  first <- p
  rest <- many (sep *> p)
  return (first : rest)


-- | Parse a template from a String
parseTemplate :: String -> Either (Doc AnsiStyle) UriTemplate
parseTemplate input =
  case runParser uriTemplate (TE.encodeUtf8 $ T.pack input) of
    OK result _ -> Right (UriTemplate result)
    Err _ -> Left (pretty ("Parse error in URI template: " ++ input))
    Fail -> Left (pretty ("Parse error in URI template: " ++ input))


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
