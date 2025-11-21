{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Error types for URI template parsing and rendering.

This module provides structured error types that can be programmatically
inspected and pattern-matched, unlike the previous Doc AnsiStyle approach.
-}
module Network.URI.Template.Error (
  -- * Parse Errors
  ParseError (..),
  displayParseError,
  displayParseErrorSimple,

  -- * Render Errors
  RenderError (..),
  displayRenderError,
) where

import Control.Exception (Exception)
import qualified Data.Text as T
import GHC.Generics (Generic)


-- | Errors that can occur when parsing a URI template.
data ParseError
  = -- | The parser encountered an unexpected character at a specific position
    UnexpectedCharacter
      { errorPosition :: !Int
      -- ^ Position in the input string where the error occurred
      , errorChar :: !Char
      -- ^ The unexpected character
      , errorContext :: !String
      -- ^ Additional context about what was expected
      }
  | -- | The template expression was not properly terminated
    UnterminatedExpression
      { errorPosition :: !Int
      -- ^ Position where the expression started
      }
  | -- | An invalid modifier character was used
    InvalidModifier
      { errorPosition :: !Int
      -- ^ Position of the invalid modifier
      , errorChar :: !Char
      -- ^ The invalid modifier character
      }
  | -- | A variable name was malformed
    MalformedVariable
      { errorPosition :: !Int
      -- ^ Position of the malformed variable
      , errorVariable :: !String
      -- ^ The malformed variable name
      }
  | -- | An invalid max length specification
    InvalidMaxLength
      { errorPosition :: !Int
      -- ^ Position of the max length specification
      , errorLengthSpec :: !String
      -- ^ The invalid length specification
      }
  | -- | Generic parse error (fallback for non-specific errors)
    GenericParseError
      { errorMessage :: !String
      -- ^ Error message
      }
  deriving (Show, Eq, Generic)


instance Exception ParseError


-- | Display a parse error with full context, including source location
-- and surrounding text for better error messages.
displayParseError :: String -> ParseError -> T.Text
displayParseError source err = case err of
  UnexpectedCharacter pos ch ctx ->
    T.pack $
      unlines
        [ "Parse error at position " ++ show pos ++ ":"
        , showSourceContext source pos
        , "Unexpected character '" ++ [ch] ++ "'"
        , ctx
        ]
  UnterminatedExpression pos ->
    T.pack $
      unlines
        [ "Parse error at position " ++ show pos ++ ":"
        , showSourceContext source pos
        , "Unterminated expression: expected '}'"
        ]
  InvalidModifier pos ch ->
    T.pack $
      unlines
        [ "Parse error at position " ++ show pos ++ ":"
        , showSourceContext source pos
        , "Invalid modifier character '" ++ [ch] ++ "'"
        , "Valid modifiers are: + # . / ; ? & ="
        ]
  MalformedVariable pos var ->
    T.pack $
      unlines
        [ "Parse error at position " ++ show pos ++ ":"
        , showSourceContext source pos
        , "Malformed variable name: " ++ var
        ]
  InvalidMaxLength pos spec ->
    T.pack $
      unlines
        [ "Parse error at position " ++ show pos ++ ":"
        , showSourceContext source pos
        , "Invalid max length specification: " ++ spec
        , "Expected a positive integer"
        ]
  GenericParseError msg ->
    T.pack $ "Parse error: " ++ msg


-- | Display a parse error in a simple format without source context.
displayParseErrorSimple :: ParseError -> T.Text
displayParseErrorSimple err = case err of
  UnexpectedCharacter pos ch ctx ->
    T.pack $ "Unexpected character '" ++ [ch] ++ "' at position " ++ show pos ++ ": " ++ ctx
  UnterminatedExpression pos ->
    T.pack $ "Unterminated expression at position " ++ show pos
  InvalidModifier pos ch ->
    T.pack $ "Invalid modifier '" ++ [ch] ++ "' at position " ++ show pos
  MalformedVariable pos var ->
    T.pack $ "Malformed variable '" ++ var ++ "' at position " ++ show pos
  InvalidMaxLength pos spec ->
    T.pack $ "Invalid max length '" ++ spec ++ "' at position " ++ show pos
  GenericParseError msg ->
    T.pack msg


-- | Show source context around an error position
showSourceContext :: String -> Int -> String
showSourceContext source pos =
  let
    contextSize = 40
    start = max 0 (pos - contextSize)
    end = min (length source) (pos + contextSize)
    before = take (pos - start) $ drop start source
    after = take (end - pos) $ drop pos source
    caretPos = length before
  in
    unlines
      [ "  " ++ before ++ after
      , "  " ++ replicate caretPos ' ' ++ "^"
      ]


-- | Errors that can occur when rendering a URI template.
data RenderError
  = -- | A required variable was not provided in the bindings
    MissingVariable
      { renderVarName :: !T.Text
      -- ^ The name of the missing variable
      }
  | -- | A variable had an incompatible value type for the template context
    InvalidValueType
      { renderVarName :: !T.Text
      -- ^ The name of the variable
      , renderTypeError :: !String
      -- ^ Description of the type mismatch
      }
  deriving (Show, Eq, Generic)


instance Exception RenderError


-- | Display a render error in a human-readable format.
displayRenderError :: RenderError -> T.Text
displayRenderError err = case err of
  MissingVariable var ->
    "Missing required variable: " <> var
  InvalidValueType var typeErr ->
    "Invalid value type for variable '" <> var <> "': " <> T.pack typeErr
