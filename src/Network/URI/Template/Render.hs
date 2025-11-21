{-# LANGUAGE ScopedTypeVariables #-}

{- |
Convenience functions for rendering URI templates to common output formats.

This module provides type-specific rendering functions that make it easier
to work with the library without needing to explicitly specify types.
-}
module Network.URI.Template.Render (
  -- * Convenience rendering functions
  renderText,
  renderLazyText,
  renderByteString,
  renderLazyByteString,
  renderString,
  renderBuilder,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.URI.Template.Internal
import Network.URI.Template.Types


-- | Render a URI template to strict 'T.Text'.
--
-- This is the most common rendering target for modern Haskell applications.
renderText :: UriTemplate -> [BoundValue] -> T.Text
renderText = render


-- | Render a URI template to lazy 'TL.Text'.
--
-- Useful for building large URIs incrementally.
renderLazyText :: UriTemplate -> [BoundValue] -> TL.Text
renderLazyText = render


-- | Render a URI template to strict 'BS.ByteString'.
--
-- Useful for network operations where ByteString is preferred.
renderByteString :: UriTemplate -> [BoundValue] -> BS.ByteString
renderByteString = render


-- | Render a URI template to lazy 'BL.ByteString'.
--
-- Useful for building large URIs or streaming operations.
renderLazyByteString :: UriTemplate -> [BoundValue] -> BL.ByteString
renderLazyByteString = render


-- | Render a URI template to 'String'.
--
-- Note: For most applications, 'renderText' is preferred for better performance.
renderString :: UriTemplate -> [BoundValue] -> String
renderString = render


-- | Render a URI template to a 'BB.Builder'.
--
-- This is the most efficient option for incremental construction or when
-- you need to combine the result with other builders.
renderBuilder :: UriTemplate -> [BoundValue] -> BB.Builder
renderBuilder = render
