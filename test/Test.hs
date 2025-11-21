{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Arrow
import Control.Monad.Writer.Strict
import qualified Data.String as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic, Rep)
import Network.URI.Template
import Network.URI.Template.Parser
import Network.URI.Template.TH
import Network.URI.Template.Types
import System.Exit
import Test.HUnit hiding (Label, path, test)
import Web.HttpApiData (toUrlPiece)


type TestRegistry = Writer [Test]


test :: Assertion -> TestRegistry ()
test t = tell [TestCase t]


suite :: TestRegistry () -> TestRegistry ()
suite t = tell [TestList $ execWriter t]


label :: String -> TestRegistry () -> TestRegistry ()
label n = censor (\l -> [TestLabel n $ TestList l])


runTestRegistry :: TestRegistry () -> IO Counts
runTestRegistry = runTestTT . TestList . execWriter


-------------------------------------------------------------------------------
-- Template Haskell Derivation Tests
-------------------------------------------------------------------------------

-- Test basic record type with default options
data Person = Person
  { personName :: Text
  , personAge :: Int
  }

$(deriveToTemplateValue ''Person)

-- Test record with field label modifier
data User = User
  { userName :: Text
  , userEmail :: Text
  }

$(deriveToTemplateValueWith defaultOptions { fieldLabelModifier = drop 4 } ''User)

-- Test record with snake_case conversion
data Product = Product
  { productName :: Text
  , productPrice :: Int
  }

$(deriveToTemplateValueWith defaultOptions { fieldLabelModifier = camelTo2 '_' } ''Product)

-- Test unary record with unwrapUnaryRecords
newtype UserId = UserId { getUserId :: Int }

$(deriveToTemplateValueWith defaultOptions { unwrapUnaryRecords = True } ''UserId)

-- Test nullary constructor
data Status = Active

$(deriveToTemplateValue ''Status)

thDerivationTests :: TestRegistry ()
thDerivationTests = label "Template Haskell Derivation Tests" $
  suite $ do
    label "Basic record with default options" $ test $ do
      let person = Person "Alice" 30
      let val = toTemplateValue person
      case val of
        Associative pairs -> do
          length pairs @?= 2
          -- Check that we have the right keys
          let keys = [toUrlPiece k | (Single k, _) <- pairs]
          keys @?= (["personName", "personAge"] :: [Text])
        _ -> assertFailure "Expected Associative value"

    label "Record with fieldLabelModifier (drop prefix)" $ test $ do
      let user = User "Bob" "bob@example.com"
      let val = toTemplateValue user
      case val of
        Associative pairs -> do
          let keys = [toUrlPiece k | (Single k, _) <- pairs]
          keys @?= (["Name", "Email"] :: [Text])
        _ -> assertFailure "Expected Associative value"

    label "Record with snake_case conversion" $ test $ do
      let product = Product "Widget" 100
      let val = toTemplateValue product
      case val of
        Associative pairs -> do
          let keys = [toUrlPiece k | (Single k, _) <- pairs]
          keys @?= (["product_name", "product_price"] :: [Text])
        _ -> assertFailure "Expected Associative value"

    label "Unary record with unwrapUnaryRecords" $ test $ do
      let userId = UserId 42
      let val = toTemplateValue userId
      case val of
        Single i -> toUrlPiece i @?= ("42" :: Text)
        _ -> assertFailure "Expected Single value"

    label "Nullary constructor" $ test $ do
      let status = Active
      let val = toTemplateValue status
      case val of
        Single s -> toUrlPiece s @?= ("Active" :: Text)
        _ -> assertFailure "Expected Single value"

    label "Use derived instance in URI template" $ test $ do
      let product = Product "Widget" 100
      let rendered = render (UriTemplate $ V.fromList [Embed Query [Variable "product" Explode]]) [("product", WrappedValue $ toTemplateValue product)]
      rendered @?= ("?product_name=Widget&product_price=100" :: Text)


-------------------------------------------------------------------------------
-- Generics Derivation Tests
-------------------------------------------------------------------------------

-- Test basic record type with generics and default options
data GenPerson = GenPerson
  { genPersonName :: Text
  , genPersonAge :: Int
  }
  deriving (Generic)

instance ToTemplateValue GenPerson where
  type TemplateRep GenPerson = GTemplateRep (Rep GenPerson)
  toTemplateValue = gToTemplateValue

-- Test record with custom generic options
data GenProduct = GenProduct
  { genProductName :: Text
  , genProductPrice :: Int
  }
  deriving (Generic)

instance ToTemplateValue GenProduct where
  type TemplateRep GenProduct = GTemplateRep (Rep GenProduct)
  toTemplateValue = gToTemplateValueWith defaultGenericOptions
    { genericFieldLabelModifier = camelTo2Generic '_'
    }

genericsDerivationTests :: TestRegistry ()
genericsDerivationTests = label "Generics Derivation Tests" $
  suite $ do
    label "Basic record with default generics" $ test $ do
      let person = GenPerson "Bob" 25
      let val = toTemplateValue person
      case val of
        Associative pairs -> do
          length pairs @?= 2
          let keys = [toUrlPiece k | (Single k, _) <- pairs]
          keys @?= (["genPersonName", "genPersonAge"] :: [Text])
        _ -> assertFailure "Expected Associative value"

    label "Record with custom field modifier (snake_case)" $ test $ do
      let product = GenProduct "Gadget" 200
      let val = toTemplateValue product
      case val of
        Associative pairs -> do
          let keys = [toUrlPiece k | (Single k, _) <- pairs]
          keys @?= (["gen_product_name", "gen_product_price"] :: [Text])
        _ -> assertFailure "Expected Associative value"

    label "Use generic instance in URI template" $ test $ do
      let product = GenProduct "Gadget" 200
      let rendered = render (UriTemplate $ V.fromList [Embed Query [Variable "product" Explode]]) [("product", WrappedValue $ toTemplateValue product)]
      rendered @?= ("?gen_product_name=Gadget&gen_product_price=200" :: Text)


main :: IO ()
main = do
  counts <- testRun
  let statusCode = errors counts + failures counts
  if statusCode > 0
    then exitFailure
    else exitSuccess
 where
  strEq = (@?=) :: Text -> Text -> Assertion
  testRun = runTestRegistry $ do
    parserTests
    showInstanceTests
    isStringInstanceTests
    quasiQuoterTests strEq
    quasiQuoterPatternTests
    embedTests
    thDerivationTests
    genericsDerivationTests
    label "RFC 6570 Core Examples" $
      suite $ do
        label "unescaped" $ test $ unescaped strEq
        label "fragment" $ test $ fragment strEq
        label "label tests" $ test $ labelTests strEq
        label "path tests" $ test $ pathTests strEq
        label "path params" $ test $ pathParams strEq
        label "query params" $ test $ queryParams strEq
        label "continued query params" $ test $ continuedQueryParams strEq
    label "RFC 6570 Section 3.2 - Comprehensive Tests" $
      suite $ do
        label "3.2.2 Simple String Expansion" $ test $ simpleExpansionTests strEq
        label "3.2.3 Reserved Expansion" $ test $ reservedExpansionTests strEq
        label "3.2.4 Fragment Expansion" $ test $ fragmentExpansionTests strEq
        label "3.2.5 Label Expansion with Dot-Prefix" $ test $ labelExpansionTests strEq
        label "3.2.6 Path Segment Expansion" $ test $ pathSegmentExpansionTests strEq
        label "3.2.7 Path-Style Parameter Expansion" $ test $ pathParameterExpansionTests strEq
        label "3.2.8 Form-Style Query Expansion" $ test $ queryExpansionTests strEq
        label "3.2.9 Form-Style Query Continuation" $ test $ queryContinuationTests strEq
    label "Edge Cases and Special Scenarios" $
      suite $ do
        label "Empty Value Handling" $ test $ emptyValueTests strEq
        label "Complex Multi-Variable Combinations" $ test $ complexCombinationTests strEq
        label "Special Character Encoding" $ test $ specialCharacterTests strEq
        label "Prefix Modifier (Length Constraints)" $ test $ prefixModifierTests strEq


parserTests :: TestRegistry ()
parserTests = label "Parser Tests" $
  suite $ do
    let foo = Variable "foo" Normal
    label "Literal" $ parserTest "foo" $ Literal "foo"
    label "Simple" $ parserTest "{foo}" $ Embed Simple [foo]
    label "Reserved" $ parserTest "{+foo}" $ Embed Reserved [foo]
    label "Fragment" $ parserTest "{#foo}" $ Embed Fragment [foo]
    label "Label" $ parserTest "{.foo}" $ Embed Label [foo]
    label "Path Segment" $ parserTest "{/foo}" $ Embed PathSegment [foo]
    label "Path Parameter" $ parserTest "{;foo}" $ Embed PathParameter [foo]
    label "Query" $ parserTest "{?foo}" $ Embed Query [foo]
    label "Query Continuation" $ parserTest "{&foo}" $ Embed QueryContinuation [foo]
    label "Explode" $ parserTest "{foo*}" $ Embed Simple [Variable "foo" Explode]
    label "Max Length" $ parserTest "{foo:1}" $ Embed Simple [Variable "foo" $ MaxLength 1]
    label "Multiple Variables" $ parserTest "{foo,bar}" $ Embed Simple [Variable "foo" Normal, Variable "bar" Normal]


parserTest :: String -> TemplateSegment -> TestRegistry ()
parserTest t e = test $ parseTemplate t @?= Right (UriTemplate (V.singleton e))


showInstanceTests :: TestRegistry ()
showInstanceTests = label "Show Instance Tests" $
  suite $ do
    label "Literal segments" $ test $ do
      show (Literal "foo") @?= "foo"
      show (Literal "/users/") @?= "/users/"

    label "Simple expansion" $ test $
      show (Embed Simple [Variable "var" Normal]) @?= "{var}"

    label "Reserved expansion" $ test $
      show (Embed Reserved [Variable "path" Normal]) @?= "{+path}"

    label "Fragment expansion" $ test $
      show (Embed Fragment [Variable "section" Normal]) @?= "{#section}"

    label "Label expansion" $ test $
      show (Embed Label [Variable "domain" Normal]) @?= "{.domain}"

    label "Path segment expansion" $ test $
      show (Embed PathSegment [Variable "segments" Normal]) @?= "{/segments}"

    label "Path parameter expansion" $ test $
      show (Embed PathParameter [Variable "params" Normal]) @?= "{;params}"

    label "Query expansion" $ test $
      show (Embed Query [Variable "filter" Normal]) @?= "{?filter}"

    label "Query continuation expansion" $ test $
      show (Embed QueryContinuation [Variable "page" Normal]) @?= "{&page}"

    label "Explode modifier" $ test $
      show (Embed Simple [Variable "list" Explode]) @?= "{list*}"

    label "MaxLength modifier" $ test $ do
      show (Embed Simple [Variable "var" (MaxLength 3)]) @?= "{var:3}"
      show (Embed Simple [Variable "var" (MaxLength 10)]) @?= "{var:10}"

    label "Multiple variables" $ test $
      show (Embed Simple [Variable "x" Normal, Variable "y" Normal]) @?= "{x,y}"

    label "Multiple variables with modifiers" $ test $ do
      show (Embed Simple [Variable "x" (MaxLength 5), Variable "y" Explode]) @?= "{x:5,y*}"
      show (Embed Query [Variable "filter" Explode, Variable "sort" Normal]) @?= "{?filter*,sort}"

    label "renderTemplate full templates" $ test $ do
      let template1 = UriTemplate (V.fromList [Literal "/users/", Embed Simple [Variable "userId" Normal]])
      renderTemplate template1 @?= "/users/{userId}"

      let template2 = UriTemplate (V.fromList [Literal "/api", Embed Query [Variable "page" Normal], Embed QueryContinuation [Variable "limit" Normal]])
      renderTemplate template2 @?= "/api{?page}{&limit}"

      let template3 = UriTemplate (V.fromList [Embed PathSegment [Variable "path" Explode], Literal "/file.txt"])
      renderTemplate template3 @?= "{/path*}/file.txt"

    label "Round-trip: parse then show" $ test $ do
      let testRoundTrip input = do
            case parseTemplate input of
              Right template -> renderTemplate template @?= input
              Left err -> assertFailure $ "Parse failed: " ++ show err

      testRoundTrip "{var}"
      testRoundTrip "{+path}"
      testRoundTrip "{#section}"
      testRoundTrip "{.domain}"
      testRoundTrip "{/segments*}"
      testRoundTrip "{;params}"
      testRoundTrip "{?query*}"
      testRoundTrip "{&continuation}"
      testRoundTrip "/users/{userId}/posts{?status,limit}"
      testRoundTrip "http://example.com{/path*}{?query*}"


isStringInstanceTests :: TestRegistry ()
isStringInstanceTests = label "IsString Instance Tests" $
  suite $ do
    label "Simple template literal" $ test $ do
      let template = S.fromString "/users/{userId}" :: UriTemplate
      renderTemplate template @?= "/users/{userId}"

    label "Template with query params" $ test $ do
      let template = S.fromString "/api{?page,limit}" :: UriTemplate
      renderTemplate template @?= "/api{?page,limit}"

    label "Complex template" $ test $ do
      let template = S.fromString "http://example.com{/path*}{?query*}{#fragment}" :: UriTemplate
      renderTemplate template @?= "http://example.com{/path*}{?query*}{#fragment}"

    label "Template with all modifier types" $ test $ do
      let template = S.fromString "{var}{+reserved}{#fragment}{.label}{/path}{;params}{?query}{&cont}" :: UriTemplate
      renderTemplate template @?= "{var}{+reserved}{#fragment}{.label}{/path}{;params}{?query}{&cont}"

    label "Template with value modifiers" $ test $ do
      let template = S.fromString "{var:3}{list*}{name:10}" :: UriTemplate
      renderTemplate template @?= "{var:3}{list*}{name:10}"

    label "Plain literal without variables" $ test $ do
      let template = S.fromString "/static/path" :: UriTemplate
      renderTemplate template @?= "/static/path"

    label "Empty template" $ test $ do
      let template = S.fromString "" :: UriTemplate
      renderTemplate template @?= ""

    label "Template can be used with render" $ test $ do
      let template = S.fromString "/users/{userId}" :: UriTemplate
      let rendered = render template [("userId", WrappedValue $ Single (123 :: Int))] :: String
      rendered @?= "/users/123"


embedTests = label "Embed Tests" $
  suite $ do
    label "Literal" $ embedTest "foo" "foo"
    label "Simple" $ embedTest "{foo}" "bar"
    label "Reserved" $ embedTest "{+foo}" "bar"
    label "Fragment" $ embedTest "{#foo}" "#bar"
    label "Label" $ embedTest "{.foo}" ".bar"
    label "Path Segment" $ embedTest "{/foo}" "/bar"
    label "Path Parameter" $ embedTest "{;foo}" ";foo=bar"
    label "Query" $ embedTest "{?foo}" "?foo=bar"
    label "Query Continuation" $ embedTest "{&foo}" "&foo=bar"
    label "Explode" $ embedTest "{foo*}" "bar"
    label "Max Length" $ embedTest "{foo:1}" "b"


embedTestEnv = [("foo", WrappedValue $ Single ("bar" :: Text))]


embedTest :: String -> String -> TestRegistry ()
embedTest t expect = test $ do
  let (Right tpl) = parseTemplate t
  let rendered = render tpl embedTestEnv
  rendered @?= expect


var :: TemplateString
var = "value"


semi :: TemplateString
semi = ";"


hello :: TemplateString
hello = "Hello World!"


path :: TemplateString
path = "/foo/bar"


list :: [TemplateString]
list = ["red", "green", "blue"]


keys :: AList TemplateString TemplateString
keys = AList [("semi", ";"), ("dot", "."), ("comma", ",")]


-- Additional RFC 6570 variables for comprehensive testing
x :: TemplateString
x = "1024"


y :: TemplateString
y = "768"


emptyStr :: TemplateString
emptyStr = ""


emptyList :: [TemplateString]
emptyList = []


emptyKeys :: AList TemplateString TemplateString
emptyKeys = AList []


who :: TemplateString
who = "fred"


base :: TemplateString
base = "http://example.com/home/"


v :: TemplateString
v = "6"


half :: TemplateString
half = "50%"


dub :: TemplateString
dub = "me/too"


count :: [TemplateString]
count = ["one", "two", "three"]


dom :: [TemplateString]
dom = ["example", "com"]


quasiQuoterTests :: (Eq s, S.IsString s, Buildable s) => (s -> s -> Assertion) -> TestRegistry ()
quasiQuoterTests (@?=) = label "QuasiQuoter Tests" $
  suite $ do
    label "Simple" $ test ([uri|{var}|] @?= "value")
    label "Simple with sufficient length" $ test ([uri|{var:20}|] @?= "value")
    label "Simple with insufficient length" $ test ([uri|{var:3}|] @?= "val")
    label "Escape" $ test ([uri|{semi}|] @?= "%3B")
    label "Escape" $ test ([uri|{semi:2}|] @?= "%3B")
    label "Multiple" $ test ([uri|{var,hello}|] @?= "value,Hello%20World%21")
    label "Length Constraint" $ test ([uri|{var:3}|] @?= "val")
    label "Sufficiently large length constraint" $ test ([uri|{var:10}|] @?= "value")
    label "List" $ test ([uri|{list}|] @?= "red,green,blue")
    label "Explode List" $ test ([uri|{list*}|] @?= "red,green,blue")
    label "Associative List" $ test ([uri|{keys}|] @?= "semi,%3B,dot,.,comma,%2C")
    label "Explode Associative List" $ test ([uri|{keys*}|] @?= "semi=%3B,dot=.,comma=%2C")
    label "Explode Associative List Query Params" $ test ([uri|{?keys*}|] @?= "?semi=%3B&dot=.&comma=%2C")


unescaped :: (Eq s, S.IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
unescaped (@?=) = do
  [uri|{+path:6}/here|] @?= "/foo/b/here"
  [uri|{+list}|] @?= "red,green,blue"
  [uri|{+list*}|] @?= "red,green,blue"
  [uri|{+keys}|] @?= "semi,;,dot,.,comma,,"
  [uri|{+keys*}|] @?= "semi=;,dot=.,comma=,"


fragment :: (Eq s, S.IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
fragment (@?=) = do
  [uri|{#path:6}/here|] @?= "#/foo/b/here"
  [uri|{#list}|] @?= "#red,green,blue"
  [uri|{#list*}|] @?= "#red,green,blue"
  [uri|{#keys}|] @?= "#semi,;,dot,.,comma,,"
  [uri|{#keys*}|] @?= "#semi=;,dot=.,comma=,"


labelTests :: (Eq s, S.IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
labelTests (@?=) = do
  [uri|X{.var:3}|] @?= "X.val"
  [uri|X{.list}|] @?= "X.red,green,blue"
  [uri|X{.list*}|] @?= "X.red.green.blue"
  [uri|X{.keys}|] @?= "X.semi,%3B,dot,.,comma,%2C"
  [uri|X{.keys*}|] @?= "X.semi=%3B.dot=..comma=%2C"


pathTests :: (Eq s, S.IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
pathTests (@?=) = do
  [uri|{/var:1,var}|] @?= "/v/value"
  [uri|{/list}|] @?= "/red,green,blue"
  [uri|{/list*}|] @?= "/red/green/blue"
  [uri|{/list*,path:4}|] @?= "/red/green/blue/%2Ffoo"
  [uri|{/keys}|] @?= "/semi,%3B,dot,.,comma,%2C"
  [uri|{/keys*}|] @?= "/semi=%3B/dot=./comma=%2C"


pathParams :: (Eq s, S.IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
pathParams (@?=) = do
  [uri|{;hello:5}|] @?= ";hello=Hello"
  [uri|{;list}|] @?= ";list=red,green,blue"
  [uri|{;list*}|] @?= ";list=red;list=green;list=blue"
  [uri|{;keys}|] @?= ";keys=semi,%3B,dot,.,comma,%2C"
  [uri|{;keys*}|] @?= ";semi=%3B;dot=.;comma=%2C"


queryParams :: (Eq s, S.IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
queryParams (@?=) = do
  [uri|{?var:3}|] @?= "?var=val"
  [uri|{?list}|] @?= "?list=red,green,blue"
  [uri|{?list*}|] @?= "?list=red&list=green&list=blue"
  [uri|{?keys}|] @?= "?keys=semi,%3B,dot,.,comma,%2C"
  [uri|{?keys*}|] @?= "?semi=%3B&dot=.&comma=%2C"


continuedQueryParams :: (Eq s, S.IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
continuedQueryParams (@?=) = do
  [uri|{&var:3}|] @?= "&var=val"
  [uri|{&list}|] @?= "&list=red,green,blue"
  [uri|{&list*}|] @?= "&list=red&list=green&list=blue"
  [uri|{&keys}|] @?= "&keys=semi,%3B,dot,.,comma,%2C"
  [uri|{&keys*}|] @?= "&semi=%3B&dot=.&comma=%2C"


-- Edge case tests for empty values
emptyValueTests :: (Eq s, S.IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
emptyValueTests (@?=) = do
  -- Empty string variable
  [uri|{emptyStr}|] @?= ""
  [uri|{?emptyStr}|] @?= "?emptyStr="
  [uri|{&emptyStr}|] @?= "&emptyStr="
  [uri|{/emptyStr}|] @?= "/"
  [uri|{;emptyStr}|] @?= ";emptyStr"
  [uri|{.emptyStr}|] @?= "."
  [uri|{#emptyStr}|] @?= "#"
  [uri|{+emptyStr}|] @?= ""
  -- Empty list
  [uri|{emptyList}|] @?= ""
  [uri|{emptyList*}|] @?= ""
  [uri|{?emptyList}|] @?= "?emptyList="
  [uri|{?emptyList*}|] @?= "?"
  [uri|{&emptyList}|] @?= "&emptyList="
  [uri|{&emptyList*}|] @?= "&"
  [uri|{/emptyList}|] @?= "/"
  [uri|{/emptyList*}|] @?= "/"
  -- Empty associative array
  [uri|{emptyKeys}|] @?= ""
  [uri|{emptyKeys*}|] @?= ""
  [uri|{?emptyKeys}|] @?= "?emptyKeys="
  [uri|{?emptyKeys*}|] @?= "?"
  [uri|{&emptyKeys}|] @?= "&emptyKeys="
  [uri|{&emptyKeys*}|] @?= "&"
  [uri|{;emptyKeys}|] @?= ";emptyKeys"
  [uri|{;emptyKeys*}|] @?= ";"
  [uri|{.emptyKeys}|] @?= "."
  [uri|{.emptyKeys*}|] @?= "."
  [uri|{/emptyKeys}|] @?= "/"
  [uri|{/emptyKeys*}|] @?= "/"


-- RFC 6570 Section 3.2.2 - Simple String Expansion
simpleExpansionTests :: (Eq s, S.IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
simpleExpansionTests (@?=) = do
  [uri|{var}|] @?= "value"
  [uri|{hello}|] @?= "Hello%20World%21"
  [uri|{half}|] @?= "50%25"
  [uri|O{emptyStr}X|] @?= "OX"
  [uri|{x,y}|] @?= "1024,768"
  [uri|{x,hello,y}|] @?= "1024,Hello%20World%21,768"
  [uri|?{x,emptyStr}|] @?= "?1024,"
  [uri|?{x,emptyStr,y}|] @?= "?1024,,768"
  [uri|{var:3}|] @?= "val"
  [uri|{var:30}|] @?= "value"
  [uri|{list}|] @?= "red,green,blue"
  [uri|{list*}|] @?= "red,green,blue"
  [uri|{keys}|] @?= "semi,%3B,dot,.,comma,%2C"
  [uri|{keys*}|] @?= "semi=%3B,dot=.,comma=%2C"


-- RFC 6570 Section 3.2.3 - Reserved Expansion
reservedExpansionTests :: (Eq s, S.IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
reservedExpansionTests (@?=) = do
  [uri|{+var}|] @?= "value"
  [uri|{+hello}|] @?= "Hello World!"
  [uri|{+half}|] @?= "50%"
  [uri|{base}index|] @?= "http%3A%2F%2Fexample.com%2Fhome%2Findex"
  [uri|{+base}index|] @?= "http://example.com/home/index"
  [uri|O{+emptyStr}X|] @?= "OX"
  [uri|{+path}/here|] @?= "/foo/bar/here"
  [uri|here?ref={+path}|] @?= "here?ref=/foo/bar"
  [uri|up{+path}{var}/here|] @?= "up/foo/barvalue/here"
  [uri|{+x,hello,y}|] @?= "1024,Hello World!,768"
  [uri|{+path,x}/here|] @?= "/foo/bar,1024/here"
  [uri|{+path:6}/here|] @?= "/foo/b/here"
  [uri|{+list}|] @?= "red,green,blue"
  [uri|{+list*}|] @?= "red,green,blue"
  [uri|{+keys}|] @?= "semi,;,dot,.,comma,,"
  [uri|{+keys*}|] @?= "semi=;,dot=.,comma=,"


-- RFC 6570 Section 3.2.4 - Fragment Expansion
fragmentExpansionTests :: (Eq s, S.IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
fragmentExpansionTests (@?=) = do
  [uri|{#var}|] @?= "#value"
  [uri|{#hello}|] @?= "#Hello World!"
  [uri|{#half}|] @?= "#50%"
  [uri|foo{#emptyStr}|] @?= "foo#"
  [uri|foo{#emptyStr,x}|] @?= "foo#,1024"
  [uri|{#x,hello,y}|] @?= "#1024,Hello World!,768"
  [uri|{#path,x}/here|] @?= "#/foo/bar,1024/here"
  [uri|{#path:6}/here|] @?= "#/foo/b/here"
  [uri|{#list}|] @?= "#red,green,blue"
  [uri|{#list*}|] @?= "#red,green,blue"
  [uri|{#keys}|] @?= "#semi,;,dot,.,comma,,"
  [uri|{#keys*}|] @?= "#semi=;,dot=.,comma=,"


-- RFC 6570 Section 3.2.5 - Label Expansion with Dot-Prefix
labelExpansionTests :: (Eq s, S.IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
labelExpansionTests (@?=) = do
  [uri|{.who}|] @?= ".fred"
  [uri|{.who,who}|] @?= ".fred.fred"
  [uri|{.half,who}|] @?= ".50%25.fred"
  [uri|www{.dom*}|] @?= "www.example.com"
  [uri|X{.var}|] @?= "X.value"
  [uri|X{.emptyStr}|] @?= "X."
  [uri|X{.x,y}|] @?= "X.1024.768"
  [uri|{.var:3}|] @?= ".val"
  [uri|{.list}|] @?= ".red,green,blue"
  [uri|{.list*}|] @?= ".red.green.blue"
  [uri|{.keys}|] @?= ".semi,%3B,dot,.,comma,%2C"
  [uri|{.keys*}|] @?= ".semi=%3B.dot=..comma=%2C"


-- RFC 6570 Section 3.2.6 - Path Segment Expansion
pathSegmentExpansionTests :: (Eq s, S.IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
pathSegmentExpansionTests (@?=) = do
  [uri|{/who}|] @?= "/fred"
  [uri|{/who,who}|] @?= "/fred/fred"
  [uri|{/half,who}|] @?= "/50%25/fred"
  [uri|{/who,dub}|] @?= "/fred/me%2Ftoo"
  [uri|{/var}|] @?= "/value"
  [uri|{/var,emptyStr}|] @?= "/value/"
  [uri|{/var,x}/here|] @?= "/value/1024/here"
  [uri|{/var:1,var}|] @?= "/v/value"
  [uri|{/list}|] @?= "/red,green,blue"
  [uri|{/list*}|] @?= "/red/green/blue"
  [uri|{/list*,path:4}|] @?= "/red/green/blue/%2Ffoo"
  [uri|{/keys}|] @?= "/semi,%3B,dot,.,comma,%2C"
  [uri|{/keys*}|] @?= "/semi=%3B/dot=./comma=%2C"


-- RFC 6570 Section 3.2.7 - Path-Style Parameter Expansion
pathParameterExpansionTests :: (Eq s, S.IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
pathParameterExpansionTests (@?=) = do
  [uri|{;who}|] @?= ";who=fred"
  [uri|{;half}|] @?= ";half=50%25"
  [uri|{;emptyStr}|] @?= ";emptyStr"
  [uri|{;v,emptyStr,who}|] @?= ";v=6;emptyStr;who=fred"
  [uri|{;x,y}|] @?= ";x=1024;y=768"
  [uri|{;x,y,emptyStr}|] @?= ";x=1024;y=768;emptyStr"
  [uri|{;hello:5}|] @?= ";hello=Hello"
  [uri|{;list}|] @?= ";list=red,green,blue"
  [uri|{;list*}|] @?= ";list=red;list=green;list=blue"
  [uri|{;keys}|] @?= ";keys=semi,%3B,dot,.,comma,%2C"
  [uri|{;keys*}|] @?= ";semi=%3B;dot=.;comma=%2C"


-- RFC 6570 Section 3.2.8 - Form-Style Query Expansion
queryExpansionTests :: (Eq s, S.IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
queryExpansionTests (@?=) = do
  [uri|{?who}|] @?= "?who=fred"
  [uri|{?half}|] @?= "?half=50%25"
  [uri|{?x,y}|] @?= "?x=1024&y=768"
  [uri|{?x,y,emptyStr}|] @?= "?x=1024&y=768&emptyStr="
  [uri|{?var:3}|] @?= "?var=val"
  [uri|{?list}|] @?= "?list=red,green,blue"
  [uri|{?list*}|] @?= "?list=red&list=green&list=blue"
  [uri|{?keys}|] @?= "?keys=semi,%3B,dot,.,comma,%2C"
  [uri|{?keys*}|] @?= "?semi=%3B&dot=.&comma=%2C"


-- RFC 6570 Section 3.2.9 - Form-Style Query Continuation
queryContinuationTests :: (Eq s, S.IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
queryContinuationTests (@?=) = do
  [uri|{&who}|] @?= "&who=fred"
  [uri|{&half}|] @?= "&half=50%25"
  [uri|?fixed=yes{&x}|] @?= "?fixed=yes&x=1024"
  [uri|{&x,y,emptyStr}|] @?= "&x=1024&y=768&emptyStr="
  [uri|{&var:3}|] @?= "&var=val"
  [uri|{&list}|] @?= "&list=red,green,blue"
  [uri|{&list*}|] @?= "&list=red&list=green&list=blue"
  [uri|{&keys}|] @?= "&keys=semi,%3B,dot,.,comma,%2C"
  [uri|{&keys*}|] @?= "&semi=%3B&dot=.&comma=%2C"


-- Complex combinations and edge cases
complexCombinationTests :: (Eq s, S.IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
complexCombinationTests (@?=) = do
  -- Multiple expansions in one template
  [uri|{var}{hello}|] @?= "valueHello%20World%21"
  [uri|{+base}{var}|] @?= "http://example.com/home/value"
  [uri|map?{x,y}&{who}|] @?= "map?1024,768&fred"
  [uri|{x,y,list}|] @?= "1024,768,red,green,blue"
  [uri|{x,y,list*}|] @?= "1024,768,red,green,blue"
  [uri|{?x,y,list}|] @?= "?x=1024&y=768&list=red,green,blue"
  [uri|{?x,y,list*}|] @?= "?x=1024&y=768&list=red&list=green&list=blue"
  -- Literals mixed with expansions
  [uri|/path/{var}/to/{who}|] @?= "/path/value/to/fred"
  [uri|{/var}{?x,y}|] @?= "/value?x=1024&y=768"
  [uri|{/var,x}{?y,who}|] @?= "/value/1024?y=768&who=fred"


-- Special character encoding tests
specialCharacterTests :: (Eq s, S.IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
specialCharacterTests (@?=) = do
  -- Special characters that need encoding in simple expansion
  [uri|{hello}|] @?= "Hello%20World%21"
  [uri|{half}|] @?= "50%25"
  [uri|{semi}|] @?= "%3B"
  [uri|{dub}|] @?= "me%2Ftoo"
  -- Same characters should be less encoded in reserved expansion
  [uri|{+hello}|] @?= "Hello World!"
  [uri|{+half}|] @?= "50%"
  [uri|{+dub}|] @?= "me/too"


-- Prefix modifier tests (length constraints)
prefixModifierTests :: (Eq s, S.IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
prefixModifierTests (@?=) = do
  -- Simple expansion
  [uri|{var:1}|] @?= "v"
  [uri|{var:3}|] @?= "val"
  [uri|{var:5}|] @?= "value"
  [uri|{var:10}|] @?= "value"
  [uri|{hello:5}|] @?= "Hello"
  [uri|{hello:15}|] @?= "Hello%20World%21"
  -- With different operators
  [uri|{+path:4}|] @?= "/foo"
  [uri|{+path:6}|] @?= "/foo/b"
  [uri|{#path:6}|] @?= "#/foo/b"
  [uri|{.var:3}|] @?= ".val"
  [uri|{/var:1,var}|] @?= "/v/value"
  [uri|{;hello:5}|] @?= ";hello=Hello"
  [uri|{?var:3}|] @?= "?var=val"
  [uri|{&var:3}|] @?= "&var=val"

-- Test quasiquoter pattern matching
quasiQuoterPatternTests :: TestRegistry ()
quasiQuoterPatternTests = label "QuasiQuoter Pattern Tests" $
  suite $ do
    label "Simple literal pattern match" $ test $ do
      let tpl = parseTemplate "/static/path"
      case tpl of
        Right [uri|/static/path|] -> return ()
        _ -> assertFailure "Pattern should match /static/path"

    label "Simple variable pattern match and binding" $ test $ do
      let tpl = parseTemplate "{userId}"
      case tpl of
        Right [uri|{userId}|] -> do
          -- userId is now bound to "userId" :: Text
          userId @?= "userId"
        _ -> assertFailure "Pattern should match {userId}"

    label "Complex pattern match with multiple bindings" $ test $ do
      let tpl = parseTemplate "/users/{userId}/posts/{postId}"
      case tpl of
        Right [uri|/users/{userId}/posts/{postId}|] -> do
          -- Both variables are bound
          userId @?= "userId"
          postId @?= "postId"
        _ -> assertFailure "Pattern should match complex template"

    label "Pattern with modifiers binds variables" $ test $ do
      let tpl = parseTemplate "{+path}{?query*}{#fragment}"
      case tpl of
        Right [uri|{+path}{?query*}{#fragment}|] -> do
          path @?= "path"
          query @?= "query"
          fragment @?= "fragment"
        _ -> assertFailure "Pattern should match template with modifiers"

    label "Pattern with max length binds variable" $ test $ do
      let tpl = parseTemplate "{var:3}"
      case tpl of
        Right [uri|{var:3}|] -> do
          var @?= "var"
        _ -> assertFailure "Pattern should match template with max length"

    label "Pattern should not match different template" $ test $ do
      let tpl = parseTemplate "/users/{userId}"
      case tpl of
        Right [uri|/posts/{postId}|] -> assertFailure "Pattern should not match different template"
        Right _ -> return ()
        Left _ -> assertFailure "Parse should succeed"

    label "Multiple variables with same modifier" $ test $ do
      let tpl = parseTemplate "{x,y,z}"
      case tpl of
        Right [uri|{x,y,z}|] -> do
          x @?= "x"
          y @?= "y"
          z @?= "z"
        _ -> assertFailure "Should match and bind all three variables"

    label "Binding with mixed literal and variable segments" $ test $ do
      let tpl = parseTemplate "/api/v1/users/{userId}/posts/{postId}/comments"
      case tpl of
        Right [uri|/api/v1/users/{userId}/posts/{postId}/comments|] -> do
          userId @?= "userId"
          postId @?= "postId"
        _ -> assertFailure "Should match complex template with mixed segments"

    label "Binding variables with all modifier types" $ test $ do
      let tpl = parseTemplate "{simple}{+reserved}{#fragment}{.label}{/path}{;param}{?query}{&cont}"
      case tpl of
        Right [uri|{simple}{+reserved}{#fragment}{.label}{/path}{;param}{?query}{&cont}|] -> do
          simple @?= "simple"
          reserved @?= "reserved"
          fragment @?= "fragment"
          label @?= "label"
          path @?= "path"
          param @?= "param"
          query @?= "query"
          cont @?= "cont"
        _ -> assertFailure "Should bind variables with all modifier types"

    label "Binding with explode modifier on different operators" $ test $ do
      let tpl = parseTemplate "{list1*}{+list2*}{#list3*}{.list4*}{/list5*}{;list6*}{?list7*}{&list8*}"
      case tpl of
        Right [uri|{list1*}{+list2*}{#list3*}{.list4*}{/list5*}{;list6*}{?list7*}{&list8*}|] -> do
          list1 @?= "list1"
          list2 @?= "list2"
          list3 @?= "list3"
          list4 @?= "list4"
          list5 @?= "list5"
          list6 @?= "list6"
          list7 @?= "list7"
          list8 @?= "list8"
        _ -> assertFailure "Should bind exploded variables"

    label "Binding with length constraints" $ test $ do
      let tpl = parseTemplate "{short:3}{medium:10}{long:100}"
      case tpl of
        Right [uri|{short:3}{medium:10}{long:100}|] -> do
          short @?= "short"
          medium @?= "medium"
          long @?= "long"
        _ -> assertFailure "Should bind variables with length constraints"

    label "Multiple variables in single expression with different modifiers" $ test $ do
      let tpl = parseTemplate "{a,b:5,c*}"
      case tpl of
        Right [uri|{a,b:5,c*}|] -> do
          a @?= "a"
          b @?= "b"
          c @?= "c"
        _ -> assertFailure "Should bind all variables despite different modifiers"

    label "Binding preserves variable name exactly" $ test $ do
      let tpl = parseTemplate "{userId123}{user_id}{camelCase}"
      case tpl of
        Right [uri|{userId123}{user_id}{camelCase}|] -> do
          userId123 @?= "userId123"
          user_id @?= "user_id"
          camelCase @?= "camelCase"
        _ -> assertFailure "Should preserve exact variable names"

    label "Pattern with only literals (no variables)" $ test $ do
      let tpl = parseTemplate "/static/path/to/resource"
      case tpl of
        Right [uri|/static/path/to/resource|] -> return ()
        _ -> assertFailure "Should match literal-only template"

    label "Empty template pattern" $ test $ do
      let tpl = parseTemplate ""
      case tpl of
        Right [uri||] -> return ()
        _ -> assertFailure "Should match empty template"

    label "Pattern with repeated variable names causes conflict (documented limitation)" $ test $ do
      -- Note: This test documents that repeated variable names in a single pattern
      -- will cause a compile error due to conflicting definitions. This is expected
      -- Haskell behavior for pattern matching.
      let tpl = parseTemplate "{id1}/{id2}"
      case tpl of
        Right [uri|{id1}/{id2}|] -> do
          id1 @?= "id1"
          id2 @?= "id2"
        _ -> assertFailure "Should bind unique variable names"

    label "Complex real-world API pattern" $ test $ do
      let tpl = parseTemplate "/api/v2/organizations/{orgId}/projects/{projectId}/issues{?status,assignee,page,limit}"
      case tpl of
        Right [uri|/api/v2/organizations/{orgId}/projects/{projectId}/issues{?status,assignee,page,limit}|] -> do
          orgId @?= "orgId"
          projectId @?= "projectId"
          status @?= "status"
          assignee @?= "assignee"
          page @?= "page"
          limit @?= "limit"
        _ -> assertFailure "Should handle complex real-world template"

    label "Pattern match failure doesn't bind variables" $ test $ do
      let tpl = parseTemplate "/users/{userId}"
      let result = case tpl of
            Right [uri|/posts/{postId}|] -> Just "matched"
            Right _ -> Nothing
            Left _ -> Nothing
      result @?= Nothing

    label "Nested pattern matching" $ test $ do
      let tpl1 = parseTemplate "/users/{userId}"
      let tpl2 = parseTemplate "/posts/{postId}"
      case (tpl1, tpl2) of
        (Right [uri|/users/{userId}|], Right [uri|/posts/{postId}|]) -> do
          userId @?= "userId"
          postId @?= "postId"
        _ -> assertFailure "Should handle nested pattern matches"

    label "Pattern with percent-encoded literals" $ test $ do
      let tpl = parseTemplate "/path%20with%20spaces/{var}"
      case tpl of
        Right [uri|/path%20with%20spaces/{var}|] -> do
          var @?= "var"
        _ -> assertFailure "Should handle percent-encoded literals"

    label "Variable bindings are type Text" $ test $ do
      let tpl = parseTemplate "{userId}"
      case tpl of
        Right [uri|{userId}|] -> do
          -- Verify it's Text by using Text operations
          let _ = T.length userId
          let _ = T.toUpper userId
          userId @?= "userId"
        _ -> assertFailure "Variables should be Text type"

    label "Multiple query parameters with continuation" $ test $ do
      let tpl = parseTemplate "/search{?q,kind,sort}{&page,limit}"
      case tpl of
        Right [uri|/search{?q,kind,sort}{&page,limit}|] -> do
          q @?= "q"
          kind @?= "kind"  -- Changed from 'type' which is a keyword
          sort @?= "sort"
          page @?= "page"
          limit @?= "limit"
        _ -> assertFailure "Should handle query parameters with continuation"

    label "Guards can use bound variables" $ test $ do
      let tpl = parseTemplate "/api/{version}/users"
      let result = case tpl of
            Right [uri|/api/{version}/users|] | version == "version" -> True
            _ -> False
      result @?= True

    label "Pattern matching in function arguments" $ test $ do
      let checkTemplate [uri|/users/{userId}|] = userId
          checkTemplate _ = "no match"
      case parseTemplate "/users/{userId}" of
        Right tpl -> checkTemplate tpl @?= "userId"
        Left _ -> assertFailure "Parse should succeed"

