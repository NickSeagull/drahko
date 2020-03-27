module IdrisCG.AutoHotkey.Generate.Common where

import IdrisCG.AutoHotkey.Syntax
import Relude

thisDot :: Expression -> Expression
thisDot =
  DotAccess (Variable $ Name "this")

nullExpr :: Expression
nullExpr = Literal (String "")

copyFunArgs :: [Name] -> Block
copyFunArgs names = do
  let copyFunArg n = [Assignment (thisDot $ Variable n) (Variable n), Assignment (Variable n) nullExpr]
  foldMap copyFunArg names
