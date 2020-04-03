import Drahko

%inline
foo : () -> ()
foo x = unsafePerformIO $ do
    msgBox "Pressed Win+Z"


main : Promise ()
main = do
  hotkey "#z" $ do
    msgBox "Pressed Win+Z"

  hotkey "#t" $ do
    msgBox "Pressed Win+T"