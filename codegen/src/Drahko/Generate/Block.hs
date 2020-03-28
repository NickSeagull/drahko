module Drahko.Generate.Block where

import Data.Data
import Drahko.Generate.Common
import qualified Drahko.Generate.Constant as Constant
import qualified Drahko.Generate.Name as Name
import Drahko.Generate.Orphans ()
import qualified Drahko.Generate.PrimFunction as PrimFunction
import Drahko.Syntax
  ( Block,
    Expression (..),
    Name (..),
    Statement (..),
  )
import qualified IRTS.Lang as Idris
import qualified Idris.Core.TT as Idris
import Relude

generate :: (Expression -> Statement) -> Idris.LExp -> Block
generate returning expression = case expression of
  Idris.LForce expr ->
    generate returning expr
  Idris.LApp _ (Idris.LV name) args -> do
    let ahkClassName = Variable (Name.generate name)
    let runName = Variable (Name "run")
    let ahkArgs = map genExpr args
    [Call (thisDot (DotAccess ahkClassName runName)) ahkArgs]
  Idris.LNothing ->
    [returning nullExpr]
  Idris.LOp primFn args -> do
    let ahkArgs = map genExpr args
    let func = PrimFunction.generate primFn ahkArgs
    [returning func]
  Idris.LForeign _ foreignName params -> do
    let ahkArgs = map (genExpr . snd) params
    genForeign returning foreignName ahkArgs
  Idris.LLet name expr restExpressions -> do
    let ahkName = Variable (Name.generate name)
    let ahkBind = generate (Assignment (thisDot ahkName)) expr
    ahkBind <> generate returning restExpressions
  Idris.LConst constExpr ->
    [returning $ Constant.generate constExpr]
  Idris.LV name -> do
    let ahkName = Name.generate name
    [returning $ thisDot (Variable ahkName)]
  Idris.LCase caseType expr alts ->
    error $
      unlines
        [ "-------- LCase ----------",
          show caseType,
          show expr,
          show alts
        ]
  otherExpression ->
    error ("\nExpression not implemented \n\n\t" <> show (toConstr otherExpression) <> "\n")

genForeign :: (Expression -> Statement) -> Idris.FDesc -> [Expression] -> Block
genForeign _ (Idris.FApp fName fArg) params =
  case (Idris.showCG fName, fArg, params) of
    ("AHK_Command", [Idris.FStr commandName], p) ->
      [Command (Name $ toText commandName) p]
    other ->
      error $
        unlines
          [ "Unsupported foreign expr",
            show fName <> show fArg,
            show params,
            "-------",
            show other
          ]
genForeign _ other _ =
  error $ unlines ["out of the case", show other]

genExpr :: Idris.LExp -> Expression
genExpr (Idris.LV name) = do
  let ahkName = Name.generate name
  thisDot (Variable ahkName)
genExpr (Idris.LApp _ (Idris.LV name) args) = do
  let ahkName = Name.generate name
  let ahkArgs = map genExpr args
  Apply (thisDot $ Variable ahkName) ahkArgs
genExpr (Idris.LCon _ _ name args) = do
  let ahkName = Name.generate name
  let ahkArgs = map genExpr args
  Apply (thisDot $ Variable ahkName) ahkArgs
genExpr (Idris.LConst c) =
  Constant.generate c
genExpr other =
  error ("\ngenExpr: " <> show (toConstr other))
