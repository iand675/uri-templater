# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.3.2.0] - Unreleased

### Added
- `ViaHttpApiData` newtype wrapper for deriving `ToTemplateValue` instances via `DerivingVia`
- Enhanced quasiquoter with pattern matching support (**UNSTABLE**: API may change in future versions)
  - Pattern matching on URI templates using `[uri|...|]` syntax
  - Variables in patterns are bound as `Text` values for use in pattern match bodies
  - Example: `case tpl of [uri|/users/{userId}|] -> print userId`
  - Note: This feature is marked unstable as we may add smarter parsing or change the binding semantics in future versions
  - Improved error messages for unsupported contexts (type and declaration)
- `Show` instance for `TemplateSegment` for debugging (#13)
- `Show` instance for `UriTemplate` for debugging (#13)
- `IsString` instance for `UriTemplate` to enable string literals (#13)
- `renderTemplate` function to convert templates back to RFC 6570 syntax (#13)
- `uriTemplateSegments` record accessor to access template segments (#13)
- Comprehensive test suite with 291 new lines of test cases covering all RFC 6570 expansion types (#11)
- 25 tests for quasiquoter pattern matching with variable binding, covering:
  - Mixed literal and variable segments
  - All modifier types (Simple, Reserved, Fragment, Label, PathSegment, PathParameter, Query, QueryContinuation)
  - Explode modifiers, length constraints, and multiple variables
  - Variable name preservation and complex real-world patterns
  - Pattern match failures, nested patterns, and guards
  - Type verification ensuring bound variables are Text
- 55 new test assertions for simple, reserved, fragment, label, path, path-parameter, query, and query-continuation expansions (#11)
- Thorough edge case testing for empty values, special character encoding, length constraints, and complex multi-variable templates (#11)
- Support for GHC 9.6, 9.8, and 9.10 (#8, #9)
- Multi-version GHC CI workflow (#9)
- Comprehensive README with usage examples and documentation (#9)
- Structured error types: `ParseError` and `RenderError` with detailed error information
- `Network.URI.Template.Error` module with error display functions:
  - `displayParseError` - Shows parse errors with source context
  - `displayParseErrorSimple` - Simple error messages without context
  - `displayRenderError` - Display render errors
- Convenience rendering functions in `Network.URI.Template.Render`:
  - `renderText` - Render to strict Text
  - `renderLazyText` - Render to lazy Text
  - `renderByteString` - Render to strict ByteString
  - `renderLazyByteString` - Render to lazy ByteString
  - `renderString` - Render to String
  - `renderBuilder` - Render to ByteString Builder
- Enhanced documentation with error handling examples and best practices

### Changed
- **BREAKING**: `UriTemplate` changed from type alias to newtype for proper `IsString` support (#13)
- **BREAKING**: `UriTemplate` now uses `Vector TemplateSegment` instead of `[TemplateSegment]` for better performance
- **BREAKING**: `parseTemplate` now returns `Either ParseError UriTemplate` instead of `Either (Doc AnsiStyle) UriTemplate`
- Migrated parser from Trifecta to flatparse for better performance and reduced dependencies (#10)
- Updated CI workflow to use modern actions and Stack-managed GHC (#9)
- Switched from ansi-wl-pprint to prettyprinter (#6)
- Modernized library structure and dependencies (#8)
- Reorganized public API exports with clear sections for core types, error handling, and advanced/internal types
- Internal implementation types (`WrappedValue`, `TemplateSegment`, `Modifier`, `ValueModifier`, `TemplateValue`) moved to "Advanced" section but still exported for backward compatibility

### Removed
- Removed trifecta, charset, and parsers dependencies in favor of flatparse (#10)
- Removed Travis CI configuration in favor of GitHub Actions (#9)
- Removed Circle CI configuration (#9)

### Fixed
- Fixed doctests for GHC 9.6+ compatibility (#8)

## [0.3.1.0] - Previous Release

(Previous changelog entries would go here)
