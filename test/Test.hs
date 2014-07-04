{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Main where
import Control.Arrow
import Control.Monad.Writer.Strict
import Network.URI.Template.Parser
import Network.URI.Template
import Network.URI.Template.TH
import Network.URI.Template.Types
import System.Exit
import Text.PrettyPrint.ANSI.Leijen (Doc, renderCompact, displayS)
import Test.HUnit hiding (test, path, Label)

-- Just being lazy here
instance Eq Doc where
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

main = do
  counts <- testRun
  let statusCode = errors counts + failures counts
  if statusCode > 0
    then exitFailure
    else exitSuccess
  where
    testRun = runTestRegistry (parserTests >> quasiQuoterTests >> embedTests)

parserTests = label "Parser Tests" $ suite $ do
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

embedTests = label "Embed Tests" $ suite $ do
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

embedTestEnv = [("foo", WrappedValue $ Single "bar")]

embedTest :: String -> String -> TestRegistry ()
embedTest t expect = test $ do
  let (Right tpl) = parseTemplate t
  let rendered = render tpl embedTestEnv
  rendered @?= expect

var :: TemplateString
var = "value"

hello :: TemplateString
hello = "Hello World!"

path :: TemplateString
path = "/foo/bar"

list :: [TemplateString]
list = ["red", "green", "blue"]

keys :: AList TemplateString TemplateString
keys = AList [("semi", ";"), ("dot", "."), ("comma", ",")]

quasiQuoterTests = label "QuasiQuoter Tests" $ suite $ do
  label "Simple" $ test ([uri|{var}|] @?= "value")
  label "Multiple" $ test ([uri|{var,hello}|] @?= "value,Hello%20World%21")
  label "Length Constraint" $ test ([uri|{var:3}|] @?= "val")
  label "Sufficiently large length constraint" $ test ([uri|{var:10}|] @?= "value")
  label "List" $ test ([uri|{list}|] @?= "red,green,blue")
  label "Explode List" $ test ([uri|{list*}|] @?= "red,green,blue")
  label "Associative List" $ test ([uri|{keys}|] @?= "semi,%3B,dot,.,comma,%2C")
  label "Explode Associative List" $ test ([uri|{?keys*}|] @?= "?semi=%3B&dot=.&comma=%2C")

unescaped = do
  [uri|{+path:6}/here|] @?= "/foo/b/here"
  [uri|{+list}|] @?= "red,green,blue"
  [uri|{+list}|] @?= "red,green,blue"
  [uri|{+keys}|] @?= "semi,;,dot,.,comma,,"
  [uri|{+keys}|] @?= "semi=;,dot=.,comma=,"

fragment = do
  [uri|{#path:6}/here|] @?= "#/foo/b/here"
  [uri|{#list}|] @?= "#red,green,blue"
  [uri|{#list*}|] @?= "#red,green,blue"
  [uri|{#keys}|] @?= "#semi,;,dot,.,comma,,"
  [uri|{#keys*}|] @?= "#semi=;,dot=.,comma=,"

labelTests = test $ do
  [uri|X{.var:3}|] @?= "X.val"
  [uri|X{.list}|] @?= "X.red,green,blue"
  [uri|X{.list*}|] @?= "X.red.green.blue"
  [uri|X{.keys}|] @?= "X.semi,%3B,dot,.,comma,%2C"
  [uri|X{.keys*}|] @?= "X.semi=%3B.dot=..comma=%2C"

pathTests = test $ do
  [uri|{/var:1,var}|] @?= "/v/value"
  [uri|{/list}|] @?= "/red,green,blue"
  [uri|{/list*}|] @?= "/red/green/blue"
  [uri|{/list*,path:4}|] @?= "/red/green/blue/%2Ffoo"
  [uri|{/keys}|] @?= "/semi,%3B,dot,.,comma,%2C"
  [uri|{/keys*}|] @?= "/semi=%3B/dot=./comma=%2C"

pathParams = do
  [uri|{;hello:5}|] @?= ";hello=Hello"
  [uri|{;list}|] @?= ";list=red,green,blue"
  [uri|{;list*}|] @?= ";list=red;list=green;list=blue"
  [uri|{;keys}|] @?= ";keys=semi,%3B,dot,.,comma,%2C"
  [uri|{;keys*}|] @?= ";semi=%3B;dot=.;comma=%2C"

queryParams = do
  [uri|{?foo}|] @?= "?foo=1"
  [uri|{?foo,bar}|] @?= "?foo=1&bar=2"
  where
    foo :: Int
    foo = 1
    bar :: Int
    bar = 2

continuedQueryParams = do
  [uri|{&foo}|] @?= "&foo=1"
  [uri|{&foo,bar}|] @?= "&foo=1&bar=2"
  where
    foo :: Int
    foo = 1
    bar :: Int
    bar = 2
