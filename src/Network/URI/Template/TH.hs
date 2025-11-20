{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Network.URI.Template.TH where

import Data.List
import qualified Data.Text as T
import qualified Data.Vector as V
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Network.URI.Template.Internal
import Network.URI.Template.Parser
import Network.URI.Template.Types


variableNames :: UriTemplate -> [T.Text]
variableNames (UriTemplate segments) = nub . V.foldr go [] $ segments
 where
  go (Literal _) l = l
  go (Embed m vs) l = map variableName vs ++ l


segmentToExpr :: TemplateSegment -> Q Exp
segmentToExpr (Literal str) = appE (conE 'Literal) (litE $ StringL $ T.unpack str)
segmentToExpr (Embed m vs) = appE (appE (conE 'Embed) modifier) $ listE $ map variableToExpr vs
 where
  modifier = do
    mname <- lookupValueName (show m)
    case mname of
      Nothing -> fail (show m ++ " is not a valid modifier")
      Just n -> conE n
  variableToExpr (Variable varName varModifier) = [|Variable $(litE $ StringL $ T.unpack varName) $(varModifierE varModifier)|]
  varModifierE vm = case vm of
    Normal -> conE 'Normal
    Explode -> conE 'Explode
    (MaxLength x) -> appE (conE 'MaxLength) $ litE $ IntegerL $ fromIntegral x


templateToExp :: UriTemplate -> Q Exp
templateToExp tpl@(UriTemplate segments) = [|render' (UriTemplate $(appE (varE 'V.fromList) (listE $ map segmentToExpr $ V.toList segments))) $(templateValues)|]
 where
  templateValues = listE $ map makePair vns
  vns = variableNames tpl
  makePair str = [|($(litE $ StringL $ T.unpack str), WrappedValue $ toTemplateValue $(varE $ mkName $ T.unpack str))|]


quasiEval :: String -> Q Exp
quasiEval str = do
  l <- location
  let parseLoc = loc_module l ++ ":" ++ show (loc_start l)
  let res = parseTemplate str
  case res of
    Left err -> fail $ show err
    Right tpl -> templateToExp tpl


-- | Parse a template and create a pattern that matches the exact template structure
-- and binds variables from the template as pattern variables.
--
-- Variables in the template become bound pattern variables that can be used in the
-- pattern match body. For example:
--
-- @
-- case template of
--   [uri|/users/{userId}/posts/{postId}|] -> (userId, postId)
--   _ -> ...
-- @
--
-- This will bind 'userId' and 'postId' as variables containing the variable names
-- from the matched template.
quasiPat :: String -> Q Pat
quasiPat str = do
  let res = parseTemplate str
  case res of
    Left err -> fail $ show err
    Right (UriTemplate segments) -> do
      -- Create a view pattern that converts Vector to list for pattern matching
      -- Pattern: UriTemplate (V.toList -> [segment1, segment2, ...])
      let segmentPats = map segmentToPat $ V.toList segments
      vecPat <- viewP (varE 'V.toList) (listP segmentPats)
      conP 'UriTemplate [return vecPat]
 where
  segmentToPat :: TemplateSegment -> Q Pat
  segmentToPat (Literal txt) = conP 'Literal [litP $ StringL $ T.unpack txt]
  segmentToPat (Embed mod vars) = conP 'Embed [modifierPat mod, listP $ map variableToPat vars]

  modifierPat :: Modifier -> Q Pat
  modifierPat m = do
    mname <- lookupValueName (show m)
    case mname of
      Nothing -> fail (show m ++ " is not a valid modifier")
      Just n -> conP n []

  variableToPat :: Variable -> Q Pat
  variableToPat (Variable name valMod) =
    -- Bind the variable name as a pattern variable
    conP 'Variable [varP $ mkName $ T.unpack name, valueModifierPat valMod]

  valueModifierPat :: ValueModifier -> Q Pat
  valueModifierPat Normal = conP 'Normal []
  valueModifierPat Explode = conP 'Explode []
  valueModifierPat (MaxLength n) = conP 'MaxLength [litP $ IntegerL $ fromIntegral n]


{- | URI quasiquoter supporting expressions and patterns.

__Expression context:__ Interpolates variables into the template at compile time.

@
let userId = 123 :: Int
url = [uri|/users/{userId}/profile|]  -- produces "/users/123/profile"
@

__Pattern context:__ Matches against a UriTemplate structure and binds variable names.

@
case parsedTemplate of
  [uri|/users/{userId}/posts/{postId}|] -> do
    -- userId and postId are now bound to their Text values
    print (userId, postId)
  _ -> putStrLn "Different template"
@

The pattern match checks that the template has the exact structure specified,
and binds each variable name from the template as a pattern variable of type 'Text'.

__Not supported:__
- Type context: URI templates are runtime values, not types
- Declaration context: Would hardcode variable names, not useful in practice
-}
uri :: QuasiQuoter
uri =
  QuasiQuoter
    { quoteExp = quasiEval
    , quotePat = quasiPat
    , quoteType = \_ -> fail "URI templates cannot be used in type context - they are runtime values, not types"
    , quoteDec = \_ -> fail "URI templates cannot be used in declaration context - this would hardcode variable names"
    }
