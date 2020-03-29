module Drahko.Generate.Block (generate) where

import Drahko.Generate.Common
import qualified Drahko.Generate.Constant as Constant
import qualified Drahko.Generate.Name as Name
import Drahko.Generate.Orphans ()
import qualified Drahko.Generate.PrimFunction as PrimFunction
import qualified Drahko.Generate.Variable as Variable
import Drahko.Syntax
import qualified IRTS.Lang as Idris
import qualified IRTS.Simplified as Idris
import qualified Idris.Core.TT as Idris
import Relude

generate :: MonadIO m => (Expression -> Statement) -> Idris.SExp -> m Block
generate returning expression = case expression of
  Idris.SApp _ name args -> do
    let ahkClassName = Variable (Name.fromName name)
    let runName = Variable (Name "run")
    let ahkArgs = map Variable.generate args
    pure [returning $ Apply (thisDot (DotAccess ahkClassName runName)) ahkArgs]
  Idris.SNothing ->
    pure [returning nullExpr]
  Idris.SOp primFn args -> do
    let ahkArgs = map Variable.generate args
    let func = PrimFunction.generate primFn ahkArgs
    pure [returning func]
  Idris.SForeign _ foreignName params -> do
    let ahkArgs = map (Variable.generate . snd) params
    pure $ genForeign returning foreignName ahkArgs
  Idris.SLet name expr restExpressions -> do
    let ahkName = Variable.generate name
    ahkBind <- generate (Assignment (thisDot ahkName)) expr
    ahkRest <- generate returning restExpressions
    pure $ ahkBind <> ahkRest
  Idris.SConst constExpr ->
    pure [returning $ Constant.generate constExpr]
  Idris.SV name -> do
    let ahkName = Variable.generate name
    pure [returning $ thisDot ahkName]
  Idris.SCon _ t _ args -> do
    let constr = Literal (Integer $ fromIntegral t)
    let ahkArgs = map Variable.generate args
    pure [returning $ Literal $ List (constr : ahkArgs)]
  Idris.SCase _ expr alts ->
    genCases returning (Variable.generate expr) alts
  Idris.SChkCase expr alts ->
    genCases returning (Variable.generate expr) alts
  otherExpression ->
    error ("\nExpression not implemented \n\n\t" <> show otherExpression <> "\n")

genCases :: Monad m => (Expression -> Statement) -> Expression -> [Idris.SAlt] -> m Block
genCases returning caseExpr alternatives = do
  let ahkCases = foldl' generateBranches ([], []) alternatives
  case ahkCases of
    ([], block) ->
      pure block
    (ifCase : elseIfCases, defaultBlock) -> do
      let elseBlock = if null defaultBlock then Nothing else Just defaultBlock
      pure [Condition (ConditionalStatement ifCase elseIfCases elseBlock)]
  where
    generateBranches :: (a, b) -> Idris.SAlt -> (a, b)
    generateBranches (cases, defaultCase) sAlt = case sAlt of
      Idris.SConstCase t expr ->
        error $
          unlines
            [ "SConstCase ",
              show t,
              show expr
            ]
      Idris.SConCase lv t _ args expr ->
        error $
          unlines
            [ "SConCase ",
              show lv,
              show t,
              show args,
              show expr
            ]
      Idris.SDefaultCase expr ->
        error $
          unlines
            [ "SDefault ",
              show expr
            ]

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
