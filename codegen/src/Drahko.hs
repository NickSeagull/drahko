module Drahko (
  codegenAHK,
  codegenAHKSimple
) where

import Drahko.StackBased
import qualified Drahko.Generate.Program as Program
import qualified Drahko.RTS as RTS
import qualified Drahko.Render as Render
import qualified IRTS.CodegenCommon as Idris
import Relude

codegenAHK :: Idris.CodeGenerator
codegenAHK Idris.CodegenInfo {..} = do
  program <- Program.generate liftDecls
  let renderedProgram = Render.renderToText $ Render.renderProgram program
  writeFile outputFile (RTS.contents <> renderedProgram)
