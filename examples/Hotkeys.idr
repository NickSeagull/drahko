import Drahko

main : Promise ()
main = do
  hotkey "#z" $ do
    msgBox "Pressed Win+Z"

  hotkey "#t" $ do
    msgBox "Pressed Win+T"
