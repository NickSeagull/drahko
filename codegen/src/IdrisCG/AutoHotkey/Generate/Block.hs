module IdrisCG.AutoHotkey.Generate.Block where

import qualified IRTS.Lang as Idris (FDesc (..), LVar (..))
import qualified IRTS.Simplified as Idris (SAlt (..), SExp (..))
import qualified IdrisCG.AutoHotkey.Generate.Constant as Constant
import qualified IdrisCG.AutoHotkey.Generate.Name as Name
import qualified IdrisCG.AutoHotkey.Generate.PrimFunction as PrimFunction
import qualified IdrisCG.AutoHotkey.Generate.Variable as Variable
import IdrisCG.AutoHotkey.Syntax
  ( BinaryOperator (..),
    Block,
    ConditionalCase (..),
    ConditionalStatement (..),
    Expression (..),
    Literal (..),
    Name (..),
    Statement (..),
  )
import Relude

generate :: (Expression -> Statement) -> Idris.SExp -> Block
generate returning expression = case expression of
  Idris.SV (Idris.Glob name) -> do
    let ahkName = Name.generate name
    [returning (Apply (Variable ahkName) [])]
  Idris.SV (Idris.Loc name) -> do
    let ahkName = Name.loc name
    [returning (Variable ahkName)]
  Idris.SApp _ functionName parameters -> do
    let ahkName = Name.generate functionName
    let ahkParams = fmap Variable.generate parameters
    let statement = Call ahkName ahkParams
    [statement]
  Idris.SLet (Idris.Loc i) v sc ->
    let ahkName = Name.loc i
     in generate (Let ahkName) v <> generate returning sc
  Idris.SNothing ->
    [returning (Literal $ String "")]
  Idris.SConst constant ->
    [returning $ Constant.generate constant]
  Idris.SOp op args -> do
    let ahkArgs = fmap Variable.generate args
    let statement = returning (PrimFunction.generate op ahkArgs)
    [statement]
  Idris.SCon _ t _ args -> do
    let con = Literal (Integer $ fromIntegral t)
    let ahkArgs = fmap Variable.generate args
    let statement = returning (Literal (List (con : ahkArgs)))
    [statement]
  Idris.SChkCase e alts -> do
    let e' = Variable.generate e
    conditionals returning e' alts
  Idris.SForeign _ f params -> do
    let params' = map (Variable.generate . snd) params
    foreign' returning f params'
  otherExpression ->
    error ("\nExpression not implemented \n\n\t" <> show otherExpression <> "\n")

foreign' :: (Expression -> Statement) -> Idris.FDesc -> [Expression] -> Block
foreign' _ (Idris.FCon name) params =
  case (show name, params) of
    (other, p) ->
      error ("\nForeign function not supported\n\n\t" <> show other <> " " <> show p)
foreign' returning (Idris.FApp name params) others =
  case (show name, params) of
    ("AHK_Function", [Idris.FStr functionName]) ->
      [returning (Apply (Variable $ Name $ toText functionName) others)]
    (other, p) ->
      error ("\nForeign function not supported\n\n\t" <> show other <> " " <> show p <> "\n\n\t" <> show others)
foreign' _ f _ = error ("Foreign function not supported: " <> show f)

conditionals :: (Expression -> Statement) -> Expression -> [Idris.SAlt] -> Block
conditionals returning caseExpr alts =
  foldl' go ([], []) alts & \case
    ([], block) -> block
    (ifCase : elseIfCases, defaultBlock) ->
      let defaults =
            if null defaultBlock
              then Nothing
              else Just defaultBlock
       in [Condition (ConditionalStatement ifCase elseIfCases defaults)]
  where
    go (cases, defaultCase) =
      \case
        Idris.SConstCase t exp' -> do
          let test' = BinaryOperatorApply Equal caseExpr $ Constant.generate t
          let block = generate returning exp'
          (cases ++ [ConditionalCase test' block], defaultCase)
        Idris.SConCase lv t _ args exp' -> do
          let t' = Literal $ Integer $ fromIntegral t
              test' = BinaryOperatorApply Equal (projectHead caseExpr) t'
              letPairs = zip [1 .. length args] [lv ..]
          let lets = map letProject letPairs
          let block = generate returning exp'
          (cases ++ [ConditionalCase test' (lets ++ block)], defaultCase)
          where
            letProject :: (Int, Int) -> Statement
            letProject (i, v) = do
              let expr = Projection caseExpr (Literal $ Integer $ fromIntegral i)
              Let (Name.loc v) expr
        Idris.SDefaultCase exp' -> do
          let block = generate returning exp'
          (cases, defaultCase ++ block)

projectHead :: Expression -> Expression
projectHead expression =
  Projection expression (Literal $ Integer 0)
