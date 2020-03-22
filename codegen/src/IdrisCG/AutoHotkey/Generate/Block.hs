module IdrisCG.AutoHotkey.Generate.Block where

import Data.Data
import qualified IRTS.Lang as Idris
import qualified IdrisCG.AutoHotkey.Generate.Name as Name
import IdrisCG.AutoHotkey.Generate.Orphans ()
import qualified IdrisCG.AutoHotkey.Generate.PrimFunction as PrimFunction
import IdrisCG.AutoHotkey.Syntax
  ( Block,
    Expression (..),
    Literal (..),
    Name (..),
    Statement (..),
  )
import Relude

generate :: (Expression -> Statement) -> Idris.LExp -> Block
generate returning expression = case expression of
  Idris.LForce expr ->
    generate returning expr
  Idris.LApp _ (Idris.LV name) args -> do
    let ahkName = Name.generate name
    let ahkArgs = map genExpr args
    [Call ahkName ahkArgs]
  Idris.LNothing ->
    [returning $ Literal (String "")]
  Idris.LOp primFn args -> do
    let ahkArgs = map genExpr args
    let func = PrimFunction.generate primFn ahkArgs
    [returning func]
  Idris.LForeign a b c -> do
    let args =
          unlines
            [ "Attempted to call FFI: ",
              show a,
              show b,
              show c
            ]
    [Call (Name "idris_putStr") [Literal $ String args]]
  otherExpression ->
    error ("\nExpression not implemented \n\n\t" <> show (toConstr otherExpression) <> "\n")

genExpr :: Idris.LExp -> Expression
genExpr (Idris.LV name) = do
  let ahkName = Name.generate name
  Variable ahkName
genExpr (Idris.LApp _ (Idris.LV name) args) = do
  let ahkName = Name.generate name
  let ahkArgs = map genExpr args
  Apply (Variable ahkName) ahkArgs
genExpr (Idris.LCon _ _ name args) = do
  let ahkName = Name.generate name
  let ahkArgs = map genExpr args
  Apply (Variable ahkName) ahkArgs
genExpr other =
  error ("\ngenExpr: " <> show (toConstr other))
