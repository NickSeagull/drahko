module IRTS.CodegenAHK where

import qualified AutoHotkey.Syntax as AHK
import qualified IRTS.CodegenCommon as Idris
import qualified Idris.Core.TT as Idris
import Relude

codegenAHK :: Idris.CodeGenerator
codegenAHK Idris.CodegenInfo {..} =
  print $ AHK.Name "Hi"

name :: Idris.Name -> AHK.Name
name = AHK.Name . toText . Idris.showCG
