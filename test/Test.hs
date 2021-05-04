{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow
import Control.Monad.Writer.Strict
import Data.String
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc)
import Network.URI.Template
import Network.URI.Template.Parser
import Network.URI.Template.TH
import Network.URI.Template.Types
import System.Exit
import Test.HUnit hiding (Label, path, test)


-- Just being lazy here
instance Eq (Doc ann) where
  (==) d1 d2 = show d1 == show d2


type TestRegistry = Writer [Test]


test :: Assertion -> TestRegistry ()
test t = tell [TestCase t]


suite :: TestRegistry () -> TestRegistry ()
suite t = tell [TestList $ execWriter t]


label :: String -> TestRegistry () -> TestRegistry ()
label n = censor (\l -> [TestLabel n $ TestList l])


runTestRegistry :: TestRegistry () -> IO Counts
runTestRegistry = runTestTT . TestList . execWriter


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
    quasiQuoterTests strEq
    embedTests
    label "RFC example cases" $
      suite $ do
        label "unescaped" $ test $ unescaped strEq
        label "fragment" $ test $ fragment strEq
        label "label tests" $ test $ labelTests strEq
        label "path tests" $ test $ pathTests strEq
        label "path params" $ test $ pathParams strEq
        label "query params" $ test $ queryParams strEq
        label "continued query params" $ test $ queryParams strEq


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
parserTest t e = test $ parseTemplate t @?= Right [e]


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


quasiQuoterTests :: (Eq s, IsString s, Buildable s) => (s -> s -> Assertion) -> TestRegistry ()
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


unescaped :: (Eq s, IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
unescaped (@?=) = do
  [uri|{+path:6}/here|] @?= "/foo/b/here"
  [uri|{+list}|] @?= "red,green,blue"
  [uri|{+list*}|] @?= "red,green,blue"
  [uri|{+keys}|] @?= "semi,;,dot,.,comma,,"
  [uri|{+keys*}|] @?= "semi=;,dot=.,comma=,"


fragment :: (Eq s, IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
fragment (@?=) = do
  [uri|{#path:6}/here|] @?= "#/foo/b/here"
  [uri|{#list}|] @?= "#red,green,blue"
  [uri|{#list*}|] @?= "#red,green,blue"
  [uri|{#keys}|] @?= "#semi,;,dot,.,comma,,"
  [uri|{#keys*}|] @?= "#semi=;,dot=.,comma=,"


labelTests :: (Eq s, IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
labelTests (@?=) = do
  [uri|X{.var:3}|] @?= "X.val"
  [uri|X{.list}|] @?= "X.red,green,blue"
  [uri|X{.list*}|] @?= "X.red.green.blue"
  [uri|X{.keys}|] @?= "X.semi,%3B,dot,.,comma,%2C"
  [uri|X{.keys*}|] @?= "X.semi=%3B.dot=..comma=%2C"


pathTests :: (Eq s, IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
pathTests (@?=) = do
  [uri|{/var:1,var}|] @?= "/v/value"
  [uri|{/list}|] @?= "/red,green,blue"
  [uri|{/list*}|] @?= "/red/green/blue"
  [uri|{/list*,path:4}|] @?= "/red/green/blue/%2Ffoo"
  [uri|{/keys}|] @?= "/semi,%3B,dot,.,comma,%2C"
  [uri|{/keys*}|] @?= "/semi=%3B/dot=./comma=%2C"


pathParams :: (Eq s, IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
pathParams (@?=) = do
  [uri|{;hello:5}|] @?= ";hello=Hello"
  [uri|{;list}|] @?= ";list=red,green,blue"
  [uri|{;list*}|] @?= ";list=red;list=green;list=blue"
  [uri|{;keys}|] @?= ";keys=semi,%3B,dot,.,comma,%2C"
  [uri|{;keys*}|] @?= ";semi=%3B;dot=.;comma=%2C"


queryParams :: (Eq s, IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
queryParams (@?=) = do
  [uri|{?var:3}|] @?= "?var=val"
  [uri|{?list}|] @?= "?list=red,green,blue"
  [uri|{?list*}|] @?= "?list=red&list=green&list=blue"
  [uri|{?keys}|] @?= "?keys=semi,%3B,dot,.,comma,%2C"
  [uri|{?keys*}|] @?= "?semi=%3B&dot=.&comma=%2C"


continuedQueryParams :: (Eq s, IsString s, Buildable s) => (s -> s -> Assertion) -> Assertion
continuedQueryParams (@?=) = do
  [uri|{&var:3}|] @?= "&var=val"
  [uri|{&list}|] @?= "&list=red,green,blue"
  [uri|{&list*}|] @?= "&list=red&list=green&list=blue"
  [uri|{&keys}|] @?= "&keys=semi,%3B,dot,.,comma,%2C"
  [uri|{&keys*}|] @?= "&semi=%3B&dot=.&comma=%2C"
