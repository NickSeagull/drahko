module IRTS.CodegenAHK (codegenAHK) where

import IRTS.CodegenCommon
import IRTS.Simplified
import Relude

codegenAHK :: CodeGenerator
codegenAHK CodegenInfo {simpleDecls} = do
  let declarations = fmap snd simpleDecls
  print declarations
