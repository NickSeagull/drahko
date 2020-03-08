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
  let renderedProgram = AHK.renderToText $ AHK.renderProgram program
  let msgBoxFunc = "MsgBox(x)\n{\n  MsgBox % x\n}\n"
  writeFile outputFile ("#Warn\n" <> msgBoxFunc <> renderedProgram)

generateProgram :: [(Idris.Name, Idris.SDecl)] -> AHK.Program
generateProgram definitions = do
  let functions = fmap generateTopLevel definitions
  let start = AHK.Call (generateName (Idris.sMN 0 "runMain")) []
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
  Idris.SOp op args -> do
    let ahkArgs = fmap generateVariable args
    let statement = returning (generatePrimitiveFunction op ahkArgs)
    [statement]
  Idris.SCon _ t _ args -> do
    let con = AHK.Literal (AHK.Integer $ fromIntegral t)
    let ahkArgs = fmap generateVariable args
    let statement = returning (AHK.Literal (AHK.List (con : ahkArgs)))
    [statement]
  Idris.SChkCase e alts -> do
    let e' = generateVariable e
    generateCases returning e' alts
  otherExpression ->
    error ("\nExpression not implemented \n\n\t" <> show otherExpression <> "\n")

generateCases :: (AHK.Expression -> AHK.Statement) -> AHK.Expression -> [Idris.SAlt] -> AHK.Block
generateCases returning caseExpr alts =
  foldl' go ([], []) alts & \case
    ([], block) -> block
    (ifCase : elseIfCases, defaultBlock) ->
      let defaults =
            if null defaultBlock
              then Nothing
              else Just defaultBlock
       in [AHK.Condition (AHK.ConditionalStatement ifCase elseIfCases defaults)]
  where
    go (cases, defaultCase) =
      \case
        Idris.SConstCase t exp' -> do
          let test' = AHK.BinaryOperatorApply AHK.Equal caseExpr $ generateConstant t
          let block = generateBody returning exp'
          (cases ++ [AHK.ConditionalCase test' block], defaultCase)
        Idris.SConCase lv t _ args exp' -> do
          let t' = AHK.Literal $ AHK.Integer $ fromIntegral t
              test' = AHK.BinaryOperatorApply AHK.Equal (projectHead caseExpr) t'
              letPairs = zip [1 .. length args] [lv ..]
          let lets = map letProject letPairs
          let block = generateBody returning exp'
          (cases ++ [AHK.ConditionalCase test' (lets ++ block)], defaultCase)
          where
            letProject :: (Int, Int) -> AHK.Statement
            letProject (i, v) = do
              let expr = AHK.Projection caseExpr (AHK.Literal $ AHK.Integer $ fromIntegral i)
              AHK.Let (locName v) expr
        Idris.SDefaultCase exp' -> do
          let block = generateBody returning exp'
          (cases, defaultCase ++ block)

projectHead :: AHK.Expression -> AHK.Expression
projectHead expression =
  AHK.Projection expression (AHK.Literal $ AHK.Integer 0)

generateConstant :: Idris.Const -> AHK.Expression
generateConstant = \case
  (Idris.I i) -> (AHK.Literal . AHK.Integer $ fromIntegral i)
  (Idris.Ch c) -> (AHK.Literal . AHK.String $ one c)
  (Idris.BI i) -> (AHK.Literal $ AHK.Integer i)
  (Idris.Str s) -> (AHK.Literal . AHK.String $ toText s)
  x | Idris.isTypeConst x -> (AHK.Literal $ AHK.String "")
  x -> error ("\nConstant not implemented \n\n\t" <> show x <> "\n")

generatePrimitiveFunction :: Idris.PrimFn -> [AHK.Expression] -> AHK.Expression
generatePrimitiveFunction Idris.LWriteStr [_, str] =
  AHK.Apply (AHK.Variable "MsgBox") [str]
generatePrimitiveFunction (Idris.LExternal n) params =
  AHK.Apply (AHK.Variable $ AHK.name (Idris.showCG n)) params
generatePrimitiveFunction x _ =
  error ("\nPrimitive function not implemented \n\n\t" <> show x <> "\n")

generateName :: Idris.Name -> AHK.Name
generateName = AHK.name . Idris.showCG

locName :: Int -> AHK.Name
locName i = AHK.Name ("loc" <> show i)

generateVariable :: Idris.LVar -> AHK.Expression
generateVariable = \case
  Idris.Loc i -> AHK.Variable (locName i)
  Idris.Glob n -> AHK.Variable (generateName n)
