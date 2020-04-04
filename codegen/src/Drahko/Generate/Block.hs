module Drahko.Generate.Block (generate) where

import Control.Monad (foldM)
import Data.Data (toConstr)
import Drahko.Generate.Common
import qualified Drahko.Generate.Constant as Constant
import Drahko.Generate.Name (ToName (..))
import qualified Drahko.Generate.Orphans ()
import Drahko.Generate.Orphans ()
import qualified Drahko.Generate.PrimFunction as PrimFunction
import qualified Drahko.Generate.Variable as Variable
import Drahko.Syntax
import qualified IRTS.Lang as Idris
-- import qualified IRTS.Simplified as Idris
import qualified Idris.Core.TT as Idris
import Relude

generate ::
  MonadState UnusedNames m =>
  MonadIO m =>
  [Name] ->
  (Expression -> Statement) ->
  Idris.LExp ->
  m Block
generate argNames returning expression = case expression of
  e@Idris.LApp {} -> do
    ahkExpr <- genExpr argNames e
    pure [returning ahkExpr]
  Idris.LNothing ->
    pure [returning nullExpr]
  Idris.LOp primFn args -> do
    ahkArgs <- traverse (genExpr argNames) args
    let func = PrimFunction.generate primFn ahkArgs
    pure [returning func]
  Idris.LForeign _ foreignName params -> do
    -- _ <- error ("\n\nLFOREIGN + \n\n" <> show params <> "\n\n" <> show foreignName)
    ahkArgs <- traverse (genExpr argNames . snd) params
    genForeign returning foreignName ahkArgs
  Idris.LLet name expr restExpressions -> do
    let ahkName = Variable.generate name
    let newScope = toName name : argNames
    ahkBind <- generate newScope (Assignment ahkName) expr
    ahkRest <- generate newScope returning restExpressions
    pure $ ahkBind <> ahkRest
  Idris.LConst constExpr ->
    pure [returning $ Constant.generate constExpr]
  Idris.LV name -> do
    setUsed name
    let ahkName = Variable.generate name
    pure [returning ahkName]
  Idris.LCase _ expr alts -> do
    ahkExpr <- genExpr argNames expr
    genCases argNames returning ahkExpr alts
  Idris.LForce (Idris.LLazyApp name args) ->
    generate argNames returning (Idris.LApp False (Idris.LV name) args)
  Idris.LForce e ->
    generate argNames returning e
  Idris.LCon _ _ name args -> do
    let ahkName = Variable.generate name
    ahkArgs <- traverse (genExpr argNames) args
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

genExpr ::
  MonadState UnusedNames m =>
  [Name] ->
  Idris.LExp ->
  m Expression
genExpr argNames (Idris.LV name) = do
  let ahkName = toName name
  if ahkName `elem` argNames
    then pure $ Variable.generate name
    else do
      setUsed name
      pure $ Variable.thisScoped name
genExpr argNames (Idris.LApp _ expr args) =
  if null args
    then genExpr argNames expr
    else do
      ahkExpr <- genExpr argNames expr
      ahkArgs <- traverse (genExpr argNames) args
      pure (scopedCall argNames ahkExpr ahkArgs)
genExpr argNames (Idris.LCon _ _ name args) =
  if Idris.showCG name == "TheWorld"
    then pure (Literal $ String "")
    else do
      setUsed name
      let ahkName = Variable.generate name
      ahkArgs <- traverse (genExpr argNames) args
      pure (Apply ahkName ahkArgs)
genExpr _ (Idris.LConst const') =
  pure (Constant.generate const')
genExpr _ Idris.LNothing =
  pure (Literal $ String "")
genExpr argNames (Idris.LOp primFn args) = do
  ahkArgs <- traverse (genExpr argNames) args
  pure (PrimFunction.generate primFn ahkArgs)
genExpr _ e =
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

genCases ::
  MonadState UnusedNames m =>
  MonadIO m =>
  [Name] ->
  (Expression -> Statement) ->
  Expression ->
  [Idris.LAlt] ->
  m Block
genCases argNames returning caseExpr alternatives = do
  ahkCases <- foldM generateBranches ([], []) alternatives
  case ahkCases of
    ([], block) ->
      pure block
    (ifCase : elseIfCases, defaultBlock) -> do
      let elseBlock = if null defaultBlock then Nothing else Just defaultBlock
      pure [Condition (ConditionalStatement ifCase elseIfCases elseBlock)]
  where
    generateBranches ::
      MonadState UnusedNames m =>
      MonadIO m =>
      ([ConditionalCase], Block) ->
      Idris.LAlt ->
      m ([ConditionalCase], Block)
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
        block <- generate argNames returning expr
        pure (cases <> [ConditionalCase test (lets <> block)], defaultCase)
      Idris.LDefaultCase expr -> do
        block <- generate argNames returning expr
        pure (cases, defaultCase <> block)

genForeign ::
  MonadIO m =>
  (Expression -> Statement) ->
  Idris.FDesc ->
  [Expression] ->
  m Block
genForeign returning (Idris.FApp fName fArg) params =
  case (Idris.showCG fName, fArg, params) of
    ("AHK_Command", [Idris.FStr commandName], p) ->
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

getName :: Expression -> Name
getName = \case
  Variable n -> n
  Apply e _ -> getName e
  _ -> ""

scopedCall :: [Name] -> Expression -> [Expression] -> Expression
scopedCall scope nameExpr args =
  if getName nameExpr `elem` scope
    then thisCall nameExpr args
    else Apply nameExpr (args <> [nullExpr])

thisCall :: Expression -> [Expression] -> Expression
thisCall nameExpr args = do
  let this = Variable.generate ("this" :: Text)
  DotAccess
    nameExpr
    ( DotAccess
        (Apply (Variable.generate ("bind" :: Text)) (this : args))
        ( Apply
            (Variable.generate ("call" :: Text))
            []
        )
    )
