import Drahko

foo : Promise String
foo = do
  let dot = "."
  let x = \y => do
    msgBox "appending"
    pure (y ++ dot)
  a <- x "Hello"
  b <- x "World"
  pure (a ++ "  ayy lmao  " ++ b)

main : Promise ()
main = do
  x <- foo
  msgBox x
  msgBox x