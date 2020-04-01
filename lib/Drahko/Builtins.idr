module Drahko.Builtins

import Drahko.FFI

%access public export

||| Call an AutoHotkey command.
%inline
call_command : String -> (ty : Type) -> {auto fty : FTy FFI_AHK [] ty} -> ty
call_command name =
  foreign FFI_AHK (AHK_Command name)

%inline
msgBox : String -> Promise ()
msgBox = call_command "MsgBox" (String -> Promise ())

%inline
hotkey : String -> () -> Promise ()
hotkey hotkeyString action =
  call_command "Hotkey" (String -> Raw (String -> ()) -> Promise ())
    hotkeyString (MkAutoHotkeyFn $ \x => action)