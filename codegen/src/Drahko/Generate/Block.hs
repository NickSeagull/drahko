module Drahko.Generate.Block (generate) where

import Control.Monad (foldM)
import Data.Data (toConstr)
import Drahko.Generate.Common
import qualified Drahko.Generate.Constant as Constant
import qualified Drahko.Generate.Orphans ()
import Drahko.Generate.Orphans ()
import qualified Drahko.Generate.PrimFunction as PrimFunction
import qualified Drahko.Generate.Variable as Variable
import Drahko.Syntax
import qualified IRTS.Lang as Idris
-- import qualified IRTS.Simplified as Idris
import qualified Idris.Core.TT as Idris
import Relude

generate :: MonadIO m => (Expression -> Statement) -> Idris.LExp -> m Block
generate returning expression = case expression of
  Idris.LApp _ expr args -> do
    ahkExpr <- genExpr expr
    ahkArgs <- traverse genExpr args
    pure [returning $ Apply ahkExpr ahkArgs]
  Idris.LNothing ->
    pure [returning nullExpr]
  Idris.LOp primFn args -> do
    ahkArgs <- traverse genExpr args
    let func = PrimFunction.generate primFn ahkArgs
    pure [returning func]
  Idris.LForeign _ foreignName params -> do
    -- _ <- error ("\n\nLFOREIGN + \n\n" <> show params <> "\n\n" <> show foreignName)
    ahkArgs <- traverse (genExpr . snd) params
    genForeign returning foreignName ahkArgs
  Idris.LLet name expr restExpressions -> do
    let ahkName = Variable.generate name
    ahkBind <- generate (Assignment ahkName) expr
    ahkRest <- generate returning restExpressions
    pure $ ahkBind <> ahkRest
  Idris.LConst constExpr ->
    pure [returning $ Constant.generate constExpr]
  Idris.LV name -> do
    let ahkName = Variable.generate name
    pure [returning ahkName]
  Idris.LCase _ expr alts -> do
    ahkExpr <- genExpr expr
    genCases returning ahkExpr alts
  Idris.LForce (Idris.LLazyApp name args) ->
    generate returning (Idris.LApp False (Idris.LV name) args)
  Idris.LForce e ->
    generate returning e
  Idris.LCon _ _ name args -> do
    let ahkName = Variable.generate name
    ahkArgs <- traverse genExpr args
    pure [returning $ Apply ahkName ahkArgs]
  otherExpression ->
    error $
      unlines
        [ "",
          "Expression not implemented",
          "",
          show (toConstr otherExpression),
          "",
          "Idris code:",
          "",
          show otherExpression,
          ""
        ]

genExpr :: Monad m => Idris.LExp -> m Expression
genExpr (Idris.LV name) =
  pure $ Variable.generate name
genExpr (Idris.LApp _ expr args) =
  if null args
    then genExpr expr
    else do
      ahkExpr <- genExpr expr
      ahkArgs <- traverse genExpr args
      pure (Apply ahkExpr ahkArgs)
genExpr (Idris.LCon _ _ name args) =
  if Idris.showCG name == "TheWorld"
    then pure (Literal $ String "")
    else do
      let ahkName = Variable.generate name
      ahkArgs <- traverse genExpr args
      pure (Apply ahkName ahkArgs)
genExpr (Idris.LConst const') =
  pure (Constant.generate const')
genExpr Idris.LNothing =
  pure (Literal $ String "")
genExpr e =
  error $
    unlines
      [ "",
        "genExpr: Expression not implemented",
        "",
        show (toConstr e),
        "",
        "Idris code:",
        "",
        show e,
        ""
      ]

genCases :: MonadIO m => (Expression -> Statement) -> Expression -> [Idris.LAlt] -> m Block
genCases returning caseExpr alternatives = do
  ahkCases <- foldM generateBranches ([], []) alternatives
  case ahkCases of
    ([], block) ->
      pure block
    (ifCase : elseIfCases, defaultBlock) -> do
      let elseBlock = if null defaultBlock then Nothing else Just defaultBlock
      pure [Condition (ConditionalStatement ifCase elseIfCases elseBlock)]
  where
    generateBranches :: MonadIO m => ([ConditionalCase], Block) -> Idris.LAlt -> m ([ConditionalCase], Block)
    generateBranches (cases, defaultCase) sAlt = case sAlt of
      Idris.LConstCase t expr ->
        error $
          unlines
            [ "SConstCase ",
              show t,
              show expr
            ]
      Idris.LConCase lv t args expr -> do
        let test = BinaryOperatorApply Equal (Projection caseExpr (Literal $ Integer 1)) (Variable.generate t)
        let letPairs = zip [2 .. length args + 1] [lv ..]
        let letProject (i, v) = Assignment (Variable.generate v) (Projection caseExpr (Literal $ Integer $ fromIntegral i))
        let lets = map letProject letPairs
        block <- generate returning expr
        pure (cases <> [ConditionalCase test (lets <> block)], defaultCase)
      Idris.LDefaultCase expr -> do
        block <- generate returning expr
        pure (cases, defaultCase <> block)

genForeign :: MonadIO m => (Expression -> Statement) -> Idris.FDesc -> [Expression] -> m Block
genForeign returning (Idris.FApp fName fArg) params = do
  putTextLn ("FOREIGN CALL: " <> show fName <> " - " <> show fArg)
  putTextLn ("ARGS: " <> show params)
  case (Idris.showCG fName, fArg, params) of
    ("AHK_Command", [Idris.FStr commandName], p) -> do
      putTextLn ("ARGS: " <> show params)
      pure [Command (Name $ toText commandName) p]
    ("AHK_Function", [Idris.FStr functionName], p) -> do
      -- we don't escape the name to let the user specify whatever
      -- function they want to call, including methods
      let funName = Variable (Name $ toText functionName)
      pure [returning $ Apply funName p]
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
