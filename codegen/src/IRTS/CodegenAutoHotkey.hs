module IRTS.CodegenAutoHotkey where

import qualified AutoHotkey.Render as AHK
import qualified AutoHotkey.Syntax as AHK
import qualified IRTS.CodegenCommon as Idris
import qualified IRTS.Lang as Idris
import qualified IRTS.Simplified as Idris
import qualified Idris.Core.TT as Idris
import Relude

codegenAHK :: Idris.CodeGenerator
codegenAHK Idris.CodegenInfo {..} = do
  let program = generateProgram simpleDecls
  putTextLn (AHK.renderToText $ AHK.renderProgram program)

generateProgram :: [(Idris.Name, Idris.SDecl)] -> AHK.Program
generateProgram definitions = do
  let functions = fmap generateTopLevel definitions
  let start = AHK.Call (AHK.name "runMain") []
  let statements = functions <> [start]
  AHK.Program statements

generateTopLevel :: (Idris.Name, Idris.SDecl) -> AHK.Statement
generateTopLevel (name, Idris.SFun _ arguments _ definition) =
  generateFunction name arguments definition

generateFunction :: Idris.Name -> [Idris.Name] -> Idris.SExp -> AHK.Statement
generateFunction functionName args definition =
  AHK.Function (generateName functionName) (map generateName args) (generateBody AHK.Return definition)

generateBody :: (AHK.Expression -> AHK.Statement) -> Idris.SExp -> AHK.Block
generateBody returning = \case
  Idris.SV (Idris.Glob name) -> do
    let ahkName = generateName name
    [returning (AHK.Apply (AHK.Variable ahkName) [])]
  Idris.SV (Idris.Loc name) -> do
    let ahkName = locName name
    [returning (AHK.Variable ahkName)]
  Idris.SApp _ functionName parameters -> do
    let ahkName = generateName functionName
    let ahkParams = fmap generateVariable parameters
    let statement = AHK.Call ahkName ahkParams
    [statement]
  Idris.SLet (Idris.Loc i) v sc ->
    do
      let ahkName = locName i
      generateBody (AHK.Let ahkName) v
      <> generateBody returning sc
  Idris.SNothing ->
    [returning (AHK.Literal $ AHK.String "")]
  Idris.SConst constant ->
    [returning $ generateConstant constant]
  otherExpression ->
    error ("\nExpression not supported \n\n\t" <> show otherExpression <> "\n")

generateConstant :: Idris.Const -> AHK.Expression
generateConstant = \case
  (Idris.I i) -> (AHK.Literal . AHK.Integer $ fromIntegral i)
  (Idris.Ch c) -> (AHK.Literal . AHK.String $ one c)
  (Idris.BI i) -> (AHK.Literal $ AHK.Integer i)
  (Idris.Str s) -> (AHK.Literal . AHK.String $ toText s)
  x | Idris.isTypeConst x -> (AHK.Literal $ AHK.String "")
  x -> error ("\nConstant not supported \n\n\t" <> show x <> "\n")

generateName :: Idris.Name -> AHK.Name
generateName = AHK.name . Idris.showCG

locName :: Int -> AHK.Name
locName i = AHK.Name ("loc" <> show i)

generateVariable :: Idris.LVar -> AHK.Expression
generateVariable = \case
  Idris.Loc i -> AHK.Variable (locName i)
  Idris.Glob n -> AHK.Variable (generateName n)
