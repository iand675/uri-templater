{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Network.URI.Template.TH
  ( -- * Quasi-quoter
    uri
    -- * Template Haskell derivation
  , deriveToTemplateValue
  , deriveToTemplateValueWith
    -- * Options
  , Options(..)
  , defaultOptions
    -- * Field name modifiers
  , camelTo2
  , camelTo
  ) where

import Control.Monad (zipWithM)
import Data.Char (isUpper, toLower, toUpper)
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


-------------------------------------------------------------------------------
-- Template Haskell Derivation
-------------------------------------------------------------------------------

-- | Options that specify how to encode/decode your datatype to/from template values.
--
-- This is designed to be similar to Aeson's 'Options' type.
data Options = Options
  { fieldLabelModifier :: String -> String
    -- ^ Function applied to field labels. Handy for removing common record prefixes for example.
  , constructorTagModifier :: String -> String
    -- ^ Function applied to constructor tags which could be used as keys in associative values.
  , omitNothingFields :: Bool
    -- ^ If 'True', fields with 'Nothing' values will be omitted from the output.
    --   Default is 'False'.
  , unwrapUnaryRecords :: Bool
    -- ^ If 'True', record constructors with a single field will have their value
    --   unwrapped instead of being wrapped in an associative value.
    --   Default is 'False'.
  }

-- | Default encoding 'Options':
--
-- @
-- 'Options'
-- { 'fieldLabelModifier'      = id
-- , 'constructorTagModifier'  = id
-- , 'omitNothingFields'       = False
-- , 'unwrapUnaryRecords'      = False
-- }
-- @
defaultOptions :: Options
defaultOptions = Options
  { fieldLabelModifier = id
  , constructorTagModifier = id
  , omitNothingFields = False
  , unwrapUnaryRecords = False
  }

-- | Derive a 'ToTemplateValue' instance for a data type.
--
-- For record types, this generates an 'Associative' template value where field names
-- are used as keys. For example:
--
-- @
-- data Person = Person { personName :: String, personAge :: Int }
-- deriveToTemplateValue ''Person
-- @
--
-- Will generate an instance that converts @Person \"Alice\" 30@ to an associative value
-- like @[(\"personName\", \"Alice\"), (\"personAge\", \"30\")]@.
--
-- Uses 'defaultOptions'.
deriveToTemplateValue :: Name -> Q [Dec]
deriveToTemplateValue = deriveToTemplateValueWith defaultOptions

-- | Like 'deriveToTemplateValue', but lets you customize the encoding options.
--
-- Example usage with custom options:
--
-- @
-- data Person = Person { personName :: String, personAge :: Int }
-- deriveToTemplateValueWith defaultOptions
--   { fieldLabelModifier = drop 6  -- Drops \"person\" prefix
--   } ''Person
-- @
deriveToTemplateValueWith :: Options -> Name -> Q [Dec]
deriveToTemplateValueWith opts name = do
  info <- reify name
  case info of
    TyConI dec -> deriveDec opts dec
    _ -> fail $ "Cannot derive ToTemplateValue for " ++ show name ++ ": not a type constructor"

deriveDec :: Options -> Dec -> Q [Dec]
deriveDec opts dec = case dec of
  DataD _ name tyVars _ cons _ -> deriveForDataType opts name (map stripTyVarBndr tyVars) cons
  NewtypeD _ name tyVars _ con _ -> deriveForDataType opts name (map stripTyVarBndr tyVars) [con]
  _ -> fail "Can only derive ToTemplateValue for data and newtype declarations"

-- | Strip the annotation from TyVarBndr to get just the name
stripTyVarBndr :: TyVarBndr flag -> Name
stripTyVarBndr (PlainTV n _) = n
stripTyVarBndr (KindedTV n _ _) = n

deriveForDataType :: Options -> Name -> [Name] -> [Con] -> Q [Dec]
deriveForDataType opts typeName tyVarNames cons = do
  let instanceType = foldl AppT (ConT typeName) (map VarT tyVarNames)

  -- For now, we only support single-constructor record types
  case cons of
    [con] -> deriveForSingleCon opts instanceType con
    _ -> fail "Currently only single-constructor types are supported for deriveToTemplateValue"

deriveForSingleCon :: Options -> Type -> Con -> Q [Dec]
deriveForSingleCon opts instanceType con = case con of
  RecC conName fields -> deriveForRecord opts instanceType conName fields
  NormalC conName [] -> deriveForNullary opts instanceType conName
  _ -> fail "Currently only record constructors and nullary constructors are supported"

deriveForRecord :: Options -> Type -> Name -> [VarBangType] -> Q [Dec]
deriveForRecord opts instanceType conName fields = do
  let fieldsInfo = [(fieldName, fieldType) | (fieldName, _, fieldType) <- fields]

  -- Check if this is a unary record and unwrapUnaryRecords is enabled
  if unwrapUnaryRecords opts && length fieldsInfo == 1
    then deriveForUnaryRecord opts instanceType conName (head fieldsInfo)
    else deriveForMultiFieldRecord opts instanceType conName fieldsInfo

deriveForUnaryRecord :: Options -> Type -> Name -> (Name, Type) -> Q [Dec]
deriveForUnaryRecord opts instanceType conName (fieldName, fieldType) = do
  valueVar <- newName "x"

  -- For unwrapped unary records, we just delegate to the field's ToTemplateValue instance
  let toTemplateValueImpl = LamE [ConP conName [] [VarP valueVar]]
        (AppE (VarE 'toTemplateValue) (VarE valueVar))

  return
    [ InstanceD Nothing [] (AppT (ConT ''ToTemplateValue) instanceType)
        [ TySynInstD (TySynEqn Nothing (AppT (ConT ''TemplateRep) instanceType) (ConT ''Single))
        , FunD 'toTemplateValue [Clause [] (NormalB toTemplateValueImpl) []]
        ]
    ]

deriveForMultiFieldRecord :: Options -> Type -> Name -> [(Name, Type)] -> Q [Dec]
deriveForMultiFieldRecord opts instanceType conName fieldsInfo = do
  varNames <- mapM (const $ newName "field") fieldsInfo

  -- Build the pattern match
  let pat = ConP conName [] (map VarP varNames)

  -- Build the list of key-value pairs
  pairExprs <- zipWithM (mkPairExpr opts) fieldsInfo varNames

  let listExpr = ListE pairExprs
  let toTemplateValueImpl = LamE [pat] (AppE (ConE 'Associative) listExpr)

  return
    [ InstanceD Nothing [] (AppT (ConT ''ToTemplateValue) instanceType)
        [ TySynInstD (TySynEqn Nothing (AppT (ConT ''TemplateRep) instanceType) (ConT ''Associative))
        , FunD 'toTemplateValue [Clause [] (NormalB toTemplateValueImpl) []]
        ]
    ]

mkPairExpr :: Options -> (Name, Type) -> Name -> Q Exp
mkPairExpr opts (fieldName, _) varName = do
  let originalFieldStr = nameBase fieldName
  let modifiedFieldStr = fieldLabelModifier opts originalFieldStr
  -- Use T.pack to convert String to Text, which has ToHttpApiData instance
  let keyExpr = AppE (ConE 'Single) (AppE (VarE 'T.pack) (LitE (StringL modifiedFieldStr)))
  let valueExpr = AppE (VarE 'toTemplateValue) (VarE varName)
  return $ TupE [Just keyExpr, Just valueExpr]

deriveForNullary :: Options -> Type -> Name -> Q [Dec]
deriveForNullary opts instanceType conName = do
  let constructorStr = nameBase conName
  let modifiedStr = constructorTagModifier opts constructorStr
  -- Use T.pack to convert String to Text
  let toTemplateValueImpl = LamE [ConP conName [] []]
        (AppE (ConE 'Single) (AppE (VarE 'T.pack) (LitE (StringL modifiedStr))))

  return
    [ InstanceD Nothing [] (AppT (ConT ''ToTemplateValue) instanceType)
        [ TySynInstD (TySynEqn Nothing (AppT (ConT ''TemplateRep) instanceType) (ConT ''Single))
        , FunD 'toTemplateValue [Clause [] (NormalB toTemplateValueImpl) []]
        ]
    ]


-------------------------------------------------------------------------------
-- Field name transformation utilities
-------------------------------------------------------------------------------

-- | Converts from camel case to another lower case, separated by the given character.
-- Has two special rules: an acronym (all uppercase) is converted to all lowercase,
-- and the first character (whether uppercase or lowercase) is never prefixed with the separator.
--
-- Example: @camelTo2 \'_\' \"SomeFieldName\" == \"some_field_name\"@
-- Example: @camelTo2 \'_\' \"productName\" == \"product_name\"@
camelTo2 :: Char -> String -> String
camelTo2 _ "" = ""
camelTo2 c (x:xs) = toLower x : go xs
  where
    go "" = ""
    go (u:l:rest) | isUpper u && isUpper l = c : toLower u : toLower l : go rest
    go (u:rest) | isUpper u = c : toLower u : go rest
    go (l:rest) = toLower l : go rest

-- | Converts from camel case to another lower case, separated by the given character.
-- Consecutive uppercase characters are not handled specially.
--
-- Example: @camelTo \'_\' \"SomeFieldName\" == \"some_field_name\"@
camelTo :: Char -> String -> String
camelTo c = go
  where
    go "" = ""
    go (x:xs) | isUpper x = c : toLower x : go xs
              | otherwise = x : go xs
