{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module URI.TH where
import Data.List
import Text.Parsec.Prim
import Text.Parsec.String
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Network.HTTP.Base
import URI.Parser
import URI.Template
import URI.Types

{-
  mTpl <- parse qqInput
  case mTpl of
    Left err -> error err
    Right tpl -> do
      varNames <- distinct <$> getAllVariableNames
      convert varNames to [(varName, toTemplateValue $ varExpr for varName)]
      render tpl convertedValues
-}

variableNames :: UriTemplate -> [String]
variableNames = nub . foldr go []
  where
    go (Literal _) l = l
    go (Embed m vs) l = map variableName vs ++ l

segmentToExpr :: TemplateSegment -> Q Exp
segmentToExpr (Literal str) = appE (conE $ mkName "URI.Types.Literal") (litE $ StringL str)
segmentToExpr (Embed m vs) = appE (appE (conE $ mkName "URI.Types.Embed") modifier) $ listE $ map variableToExpr vs
  where
    modifier = conE $ mkName ("URI.Types." ++ show m)
    variableToExpr (Variable varName varModifier) = [| Variable $(litE $ StringL varName) $(varModifierE varModifier) |]
    varModifierE vm = case vm of
      Normal -> conE $ mkName "URI.Types.Normal"
      Explode -> conE $ mkName "URI.Types.Explode"
      (MaxLength x) -> appE (conE $ mkName "URI.Types.MaxLength") $ litE $ IntegerL $ fromIntegral x

templateToExp :: UriTemplate -> Q Exp
templateToExp ts = [| render $(listE $ map segmentToExpr ts) $(templateValues) |]
  where
    templateValues = listE $ map makePair vns
    vns = variableNames ts
    makePair str = [| ($(litE $ StringL str), internalize $ toTemplateValue $ $(varE $ mkName str)) |]

-- AppE (VarE 'concat) $ ListE $ concatMap segmentToExp ts

{-segmentToExp (Literal s) = [LitE $ StringL s]-}
{-segmentToExp (Embed m v) = map (AppE prefix . enc . VarE . mkName) v-}
  {-where-}
    {-enc = AppE (VarE $ encoder m)-}
    {--- cons the prefix onto the beginning of each embedded segment-}
    {-prefix = InfixE (Just $ LitE $ CharL $ subsequentSeparator m) (ConE $ '(:)) Nothing-}

quasiEval :: String -> Q Exp
quasiEval str = do
  l <- location
  let parseLoc = loc_module l ++ ":" ++ show (loc_start l)
  let res = parse uriTemplate parseLoc str
  case res of
    Left err -> fail $ show err
    Right tpl -> templateToExp tpl

uri :: QuasiQuoter
uri = QuasiQuoter
  { quoteExp = quasiEval
  , quotePat = error "Cannot use uri quasiquoter in pattern"
  , quoteType = error "Cannot use uri quasiquoter in type"
  , quoteDec = error "Cannot use uri quasiquoter as declarations"
  }
