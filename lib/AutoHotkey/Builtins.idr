module AutoHotkey.Builtins

import AutoHotkey.FFI

%access public export

||| Call an AutoHotkey command.
%inline
call_command : String -> (ty : Type) -> {auto fty : FTy FFI_AHK [] ty} -> ty
call_command name =
  foreign FFI_AHK (AHK_Command name)

%inline
msgBox : String -> AHK_IO ()
msgBox = call_command "MsgBox" (String -> AHK_IO ())
