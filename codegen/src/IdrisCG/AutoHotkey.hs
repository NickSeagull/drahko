module IdrisCG.AutoHotkey where

import qualified IRTS.CodegenCommon as Idris
import qualified IdrisCG.AutoHotkey.Generate.Program as Program
import qualified IdrisCG.AutoHotkey.RTS as RTS
import qualified IdrisCG.AutoHotkey.Render as Render
import Relude

codegenAHK :: Idris.CodeGenerator
codegenAHK Idris.CodegenInfo {..} = do
  let program = Program.generate liftDecls
  let renderedProgram = Render.renderToText $ Render.renderProgram program
  writeFile outputFile (RTS.contents <> renderedProgram)
