foo : String
foo =
  let x = \y => y ++ " hello"
  in x "world"

main : IO ()
main = putStrLn foo