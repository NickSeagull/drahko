module IdrisCG.AutoHotkey.Generate.Constant where

import Idris.Core.TT as Idris (Const (..), isTypeConst)
import IdrisCG.AutoHotkey.Syntax (Expression (..), Literal (..))
import Relude

generate :: Idris.Const -> Expression
generate = \case
  (Idris.I i) -> (Literal . Integer $ fromIntegral i)
  (Idris.Ch c) -> (Literal . String $ one c)
  (Idris.BI i) -> (Literal $ Integer i)
  (Idris.Str s) -> (Literal . String $ toText s)
  x | isTypeConst x -> (Literal $ String "")
  x -> error ("\nConstant not implemented \n\n\t" <> show x <> "\n")
