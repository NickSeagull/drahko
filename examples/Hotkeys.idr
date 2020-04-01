import Drahko

%inline
foo : () -> ()
foo x = unsafePerformIO $ do
    msgBox "Pressed Win+Z"


main : Promise ()
main = do
  hotkey "#z" $ \_ =>
    msgBox "Pressed Win+Z"

  hotkey "#t" $ \_ =>
    msgBox "Pressed Win+T"