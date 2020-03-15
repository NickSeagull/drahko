module AutoHotkey.Builtins

import AutoHotkey.FFI

%access public export
%default total

data AutoHotkeyList a = MkList (Raw a)

data Key
  = Ctrl
  | Alt
  | U
  | W

||| Call an AutoHotkey builtin.
%inline
builtin : String -> (ty : Type) -> {auto fty : FTy FFI_AHK [] ty} -> ty
builtin name =
  foreign FFI_AHK (AHK_Function name)

%inline
msgBox : String -> AHK_IO ()
msgBox = builtin "MsgBox" (String -> AHK_IO ())

twice : (Int -> Int) -> Int -> AHK_IO Int
twice f x =
  foreign FFI_AHK (AHK_Function "twice")
    (AutoHotkeyFn (Int -> Int) -> Int -> AHK_IO Int)
    (MkAutoHotkeyFn f)
    x