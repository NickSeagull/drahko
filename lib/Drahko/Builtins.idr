module Drahko.Builtins

import Drahko.FFI

%access public export



%inline
msgBox : String -> Promise ()
msgBox = call_command "MsgBox" (String -> Promise ())

%inline
hotkey : String -> Promise () -> Promise ()
hotkey hotkeyString action =
  call_function "__drahko.hotkey"
    (String -> (() -> Promise ()) -> Promise ())
    hotkeyString (\_ => action)

%inline
reload : () -> Promise ()
reload = (call_command "Reload" (() -> Promise ()))

%inline
tooltip : String -> Promise ()
tooltip = call_command "Tooltip" (String -> Promise ())

%inline
sendInput : String -> Promise ()
sendInput = call_command "SendInput" (String -> Promise ())

%inline
sleep : Int -> Promise ()
sleep = call_command "Sleep" (Int -> Promise ())

%inline
loop : Promise () -> Promise ()
loop fun = call_function "__drahko._loop" ((() -> Promise ()) -> Promise ()) (\_ => fun)