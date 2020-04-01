import Drahko

%inline
foo : () -> ()
foo x = unsafePerformIO $ do
    msgBox "Pressed Win+Z"


main : Promise ()
main = do
  foreign FFI_AHK
    (AHK_Command "Hotkey")
    (String -> AutoHotkeyFn (() -> ()) -> Promise ())
    "#z"
    (MkAutoHotkeyFn foo)

  pure ()