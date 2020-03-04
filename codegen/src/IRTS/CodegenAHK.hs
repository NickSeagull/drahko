module IRTS.CodegenAHK (codegenAHK) where

import IRTS.CodegenCommon
import Relude

codegenAHK :: CodeGenerator
codegenAHK CodegenInfo {simpleDecls} = do
  let declarations = fmap fst simpleDecls
  print declarations
