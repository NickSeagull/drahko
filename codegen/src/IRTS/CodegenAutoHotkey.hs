module IRTS.CodegenAutoHotkey where

import qualified AutoHotkey.Syntax as AHK
import qualified IRTS.CodegenCommon as Idris
import qualified IRTS.Lang as Idris
import qualified IRTS.Simplified as Idris
import qualified Idris.Core.TT as Idris
import Relude

codegenAHK :: Idris.CodeGenerator
codegenAHK Idris.CodegenInfo {..} =
  print $ AHK.Name "Hi"

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
  otherExpression ->
    error ("Expression not supported " <> show otherExpression)

generateName :: Idris.Name -> AHK.Name
generateName = AHK.name . Idris.showCG

locName :: Int -> AHK.Name
locName i = AHK.Name (show i)

generateVariable :: Idris.LVar -> AHK.Expression
generateVariable = \case
  Idris.Loc i -> AHK.Variable (locName i)
  Idris.Glob n -> AHK.Variable (generateName n)
