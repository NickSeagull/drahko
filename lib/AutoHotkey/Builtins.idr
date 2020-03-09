module AutoHotkey.Builtins

%access public export
%default total

msgBox : String -> IO ()
msgBox = putStr