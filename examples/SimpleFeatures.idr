import Drahko

foo : Promise String
foo = do
  let x = \y => do
    msgBox "appending"
    pure (y ++ ".")
  a <- x "Hello"
  b <- x "World"
  pure (a ++ b)

main : Promise ()
main = do
  x <- foo
  msgBox x
  msgBox x