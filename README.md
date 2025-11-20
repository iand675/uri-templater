# uri-templater

[![build](https://github.com/iand675/uri-templater/actions/workflows/ci.yml/badge.svg)](https://github.com/iand675/uri-templater/actions/workflows/ci.yml)

A Haskell implementation of [RFC 6570 URI Templates](https://tools.ietf.org/html/rfc6570) with compile-time quasi-quotation support.

## Overview

URI templates provide a standardized way to construct URIs with variable substitution, automatic escaping, and flexible formatting for different URI components. This library offers:

- **Compile-time quasi-quoters** for type-safe URI template construction
- **RFC 6570 compliant** parser and renderer
- **Multiple output formats** supporting Text, ByteString, String, and more
- **Extensive type support** including lists, maps, and custom types via `ToTemplateValue`
- **Flexible escaping** appropriate for each URI component

## Installation

Add to your `package.yaml` or `.cabal` file:

```yaml
dependencies:
  - uri-templater
```

Or using Stack:

```bash
stack install uri-templater
```

## Quick Start

```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Network.URI.Template

-- Simple variable substitution
userName :: Text
userName = "alice"

-- Renders to: "/users/alice"
userPath = [uri|/users/{userName}|]

-- Query parameters with lists
tags :: [Text]
tags = ["haskell", "web", "api"]

-- Renders to: "/search?tags=haskell&tags=web&tags=api"
searchPath = [uri|/search{?tags*}|]
```

## Template Syntax

URI templates use curly braces `{}` for variable expansion with different operators for different URI components:

### Simple String Expansion

```haskell
let var = "value" :: TemplateString

[uri|{var}|]           -- "value"
[uri|{var:3}|]         -- "val" (prefix modifier)
[uri|{x,y}|]           -- "value1,value2" (multiple values)
```

### Reserved Character Expansion (`+`)

Allows reserved URI characters to pass through unescaped:

```haskell
let path = "/foo/bar" :: TemplateString

[uri|{+path}|]         -- "/foo/bar" (unescaped)
```

### Fragment Expansion (`#`)

```haskell
[uri|{#section}|]      -- "#introduction"
```

### Label Expansion (`.`)

Useful for domain names or file extensions:

```haskell
let domain = "example" :: TemplateString

[uri|{.domain}|]       -- ".example"
[uri|X{.list*}|]       -- "X.red.green.blue" (exploded list)
```

### Path Segment Expansion (`/`)

```haskell
let segments = ["users", "123", "profile"] :: [TemplateString]

[uri|{/segments*}|]    -- "/users/123/profile"
```

### Path Parameter Expansion (`;`)

```haskell
[uri|{;params*}|]      -- ";key1=val1;key2=val2"
```

### Query Parameter Expansion (`?` and `&`)

```haskell
let filters = [("status", "active"), ("limit", "10")] :: [(Text, Text)]

[uri|{?filters*}|]     -- "?status=active&limit=10"
[uri|/api{?page}{&filters*}|]  -- "/api?page=1&status=active&limit=10"
```

## Variable Modifiers

### Prefix Modifier (`:n`)

Limits the variable to the first `n` characters:

```haskell
[uri|{var:3}|]         -- "val" (from "value")
```

### Explode Modifier (`*`)

Expands lists and associative arrays:

```haskell
let list = ["red", "green", "blue"] :: [TemplateString]
let keys = AList [("semi", ";"), ("dot", ".")] :: AList TemplateString TemplateString

[uri|{list}|]          -- "red,green,blue"
[uri|{list*}|]         -- "red,green,blue"
[uri|{/list*}|]        -- "/red/green/blue"
[uri|{?list*}|]        -- "?list=red&list=green&list=blue"

[uri|{keys}|]          -- "semi,%3B,dot,."
[uri|{keys*}|]         -- "semi=%3B,dot=."
```

## Working with Custom Types

Implement `ToTemplateValue` for your types:

```haskell
import Network.URI.Template

data User = User
  { userId :: Int
  , userName :: Text
  }

instance ToTemplateValue User where
  toTemplateValue user = toTemplateValue (userName user)

-- Or for associative representation:
instance ToTemplateValue User where
  toTemplateValue user = AssociativeList
    [ ("id", toTemplateValue $ userId user)
    , ("name", toTemplateValue $ userName user)
    ]
```

## Manual Template Construction

For dynamic templates, use the parser and renderer:

```haskell
import Network.URI.Template.Parser
import Network.URI.Template

-- Parse a template
template <- case parseTemplate "/users/{userId}/posts{?status}" of
  Left err -> error $ show err
  Right tpl -> return tpl

-- Render with variables
let vars = [ ("userId", WrappedValue $ Single (123 :: Int))
           , ("status", WrappedValue $ Single ("published" :: Text))
           ]

render template vars  -- "/users/123/posts?status=published"
```

## Multiple Output Formats

The library supports multiple string types through the `Buildable` typeclass:

```haskell
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

textUri :: T.Text
textUri = [uri|/path/{var}|]

strictBS :: BS.ByteString
strictBS = [uri|/path/{var}|]

lazyBS :: BL.ByteString
lazyBS = [uri|/path/{var}|]
```

## Supported GHC Versions

- GHC 9.6.7
- GHC 9.8.4
- GHC 9.10.3

## Security Considerations

URI templates automatically handle proper escaping for each URI component. However:

- Validate user input before using it in templates
- Be cautious when using the `+` (reserved) operator with untrusted input
- Consider the security implications of the URI context where templates are expanded

See [RFC 6570 Section 6](https://tools.ietf.org/html/rfc6570#section-6) for detailed security considerations.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Author

Ian Duncan ([@iand675](https://github.com/iand675))

## References

- [RFC 6570: URI Template](https://tools.ietf.org/html/rfc6570)
- [Hackage Documentation](https://hackage.haskell.org/package/uri-templater)
