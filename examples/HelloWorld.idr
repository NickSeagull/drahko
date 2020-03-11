import AutoHotkey.Builtins

foo : String
foo =
  let x = \y => y ++ "from Idris!"
  in x "Hello AutoHotkey, "

main : IO ()
main = msgBox foo