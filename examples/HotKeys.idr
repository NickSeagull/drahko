import AutoHotkey.Builtins
import AutoHotkey.FFI

main : AHK_IO ()
main = do
  hotkey "#x" $ do
    msgBox "winx"

  hotkey "^x" $ do
    msgBox "ctrlx"