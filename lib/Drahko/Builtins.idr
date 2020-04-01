module Drahko.Builtins

import Drahko.FFI

%access public export



%inline
msgBox : String -> Promise ()
msgBox = call_command "MsgBox" (String -> Promise ())

%inline
hotkey : String -> Promise () -> Promise ()
hotkey hotkeyString action =
  call_function
          "Hotkey"
          (String -> (() -> Promise ()) -> Promise ())
          hotkeyString (\_ => action)