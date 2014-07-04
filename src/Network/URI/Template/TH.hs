{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Network.URI.Template.TH where
import Data.List
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Network.HTTP.Base
import Network.URI.Template.Internal
import Network.URI.Template.Parser
import Network.URI.Template.Types

variableNames :: UriTemplate -> [String]
variableNames = nub . foldr go []
  where
    go (Literal _) l = l
    go (Embed m vs) l = map variableName vs ++ l

segmentToExpr :: TemplateSegment -> Q Exp
segmentToExpr (Literal str) = appE (conE 'Literal) (litE $ StringL str)
segmentToExpr (Embed m vs) = appE (appE (conE 'Embed) modifier) $ listE $ map variableToExpr vs
  where
    modifier = do
      mname <- lookupValueName (show m)
      case mname of
        Nothing -> fail (show m ++ " is not a valid modifier")
        Just n -> conE n
    variableToExpr (Variable varName varModifier) = [| Variable $(litE $ StringL varName) $(varModifierE varModifier) |]
    varModifierE vm = case vm of
      Normal -> conE 'Normal
      Explode -> conE 'Explode
      (MaxLength x) -> appE (conE 'MaxLength) $ litE $ IntegerL $ fromIntegral x

templateToExp :: UriTemplate -> Q Exp
templateToExp ts = [| render' $(listE $ map segmentToExpr ts) $(templateValues) |]
  where
    templateValues = listE $ map makePair vns
    vns = variableNames ts
    makePair str = [| ($(litE $ StringL str), WrappedValue $ toTemplateValue $ $(varE $ mkName str)) |]

quasiEval :: String -> Q Exp
quasiEval str = do
  l <- location
  let parseLoc = loc_module l ++ ":" ++ show (loc_start l)
  let res = parseTemplate str
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
