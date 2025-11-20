{- |
  RFC 6570 URI templates are a convenient mechanism for constructing URIs in
  a standardized way. They provide the ability to escape the interpolated values appropriately,
  format them for the appropriate part of the URI, and can handle list-like (@[]@, @Vector@)
  and associative inputs (such as associative lists, @Map@s, @HashMap@s).

  This implementation supports outputting these URI fragments to all widely-used textual/bytestring
  formats, and provides basic facilities for extending the URI rendering to support other output formats.
-}
module Network.URI.Template (
  -- $overview

  -- * Quasi-Quoting URI templates
  uri,

  -- * Manually parsing, constructing, & writing URI templates
  render,
  ToTemplateValue (..),
  AList (..),
  TemplateString (..),
  parseTemplate,
  renderTemplate,
  UriTemplate (..),
  TemplateSegment (..),
  Modifier (..),
  ValueModifier (..),
  TemplateValue (..),

  -- * Implementing a new output format for URI templates
  Buildable (..),
) where

import Network.URI.Template.Internal
import Network.URI.Template.Parser
import Network.URI.Template.TH
import Network.URI.Template.Types


{- $overview

Due to the large number of different interpolation options available, it is arguably easiest
to view an example of each sort of interpolation to see how it works.
RFC 6570 itself <https://tools.ietf.org/html/rfc6570> also
provides a large number of these same examples, so it may help to look at it directly.

$setup
>>> :set -XQuasiQuotes
>>> :set -XOverloadedStrings
>>> import Network.URI.Template
>>> let var = "value" :: TemplateString
>>> let semi = ";" :: TemplateString
>>> let hello = "Hello World!" :: TemplateString
>>> let path = "/foo/bar" :: TemplateString
>>> let list = ["red", "green", "blue"] :: [TemplateString]
>>> let keys = AList [("semi", ";"), ("dot", "."), ("comma", ",")] :: AList TemplateString TemplateString

= Simple interpolation

Simple string expansion is the default expression type when no
operator is given.

For each defined variable in the variable-list, perform variable
expansion. If more than one variable has a
defined value, append a comma (",") to the result string as a
separator between variable expansions.

== Without modifiers

>>> [uri|{var}|]
"value"

== Interpolating with a length modifier

>>> [uri|{var:3}|]
"val"

== Interpolating multiple values:

>>> [uri|{var,hello}|]
"value,Hello%20World%21"

== Interpolating a list:

>>> [uri|{list}|]
"red,green,blue"

== Exploding a list (not super useful without modifiers):

>>> [uri|{list*}|]
"red,green,blue"

== Interpolating associative values:

>>> [uri|{keys}|]
"semi,%3B,dot,.,comma,%2C"

== Exploding associative values

>>> [uri|{keys*}|]
"semi=%3B,dot=.,comma=%2C"

= Unescaped interpolation

>>> [uri|{+path:6}/here|]
"/foo/b/here"

>>> [uri|{+list}|]
"red,green,blue"

>>> [uri|{+list*}|]
"red,green,blue"

>>> [uri|{+keys}|]
"semi,;,dot,.,comma,,"

>>> [uri|{+keys*}|]
"semi=;,dot=.,comma=,"

= Path piece interpolation

Path segment expansion, as indicated by the slash ("/"), is useful for describing URI path
hierarchies.

For each defined variable in the variable-list, append "/" to the
result string and then perform variable expansion.

Note that the expansion process for path segment expansion is
identical to that of label expansion aside from the substitution of
"/" instead of ".".  However, unlike ".", a "/" is a reserved
character and will be percent-encoded if found in a value.

>>> [uri|{/var:1,var}|]
"/v/value"

>>> [uri|{/list}|]
"/red,green,blue"

>>> [uri|{/list*}|]
"/red/green/blue"

= Query param interpolation

>>> [uri|{?var:3}|]
"?var=val"

>>> [uri|{?list}|]
"?list=red,green,blue"

>>> [uri|{?list*}|]
"?list=red&list=green&list=blue"

>>> [uri|{?keys}|]
"?keys=semi,%3B,dot,.,comma,%2C"

>>> [uri|{?keys*}|]
"?semi=%3B&dot=.&comma=%2C"

= Continued query param interpolation

>>> [uri|{?var:3}|]
"?var=val"

>>> [uri|{?list}|]
"?list=red,green,blue"

>>> [uri|{?list*}|]
"?list=red&list=green&list=blue"

>>> [uri|{?keys}|]
"?keys=semi,%3B,dot,.,comma,%2C"

>>> [uri|{?keys*}|]
"?semi=%3B&dot=.&comma=%2C"

= Label interpolation

Label expansion, as indicated by the dot (".") operator is useful for describing URI spaces with varying
domain names or path selectors (e.g., filename extensions).

>>> [uri|X{.var:3}|]
"X.val"

>>> [uri|X{.list}|]
"X.red,green,blue"

>>> [uri|X{.list*}|]
"X.red.green.blue"

>>> [uri|X{.keys}|]
"X.semi,%3B,dot,.,comma,%2C"

>>> [uri|X{.keys*}|]
"X.semi=%3B.dot=..comma=%2C"

= Fragment interpolation

Path-style parameter expansion, as indicated by the semicolon (";")
is useful for describing URI path parameters, such as "path;property" or "path;name=value".

>>> [uri|{;hello:5}|]
";hello=Hello"

>>> [uri|{;list}|]
";list=red,green,blue"

>>> [uri|{;list*}|]
";list=red;list=green;list=blue"

>>> [uri|{;keys}|]
";keys=semi,%3B,dot,.,comma,%2C"

>>> [uri|{;keys*}|]
";semi=%3B;dot=.;comma=%2C"

= Security Considerations

A URI Template does not contain active or executable content.
However, it might be possible to craft unanticipated URIs if an
attacker is given control over the template or over the variable
values within an expression that allows reserved characters in the
expansion.  In either case, the security considerations are largely
determined by who provides the template, who provides the values to
use for variables within the template, in what execution context the
expansion occurs (client or server), and where the resulting URIs are
used.
-}
