import Drahko

foo : Promise ()
foo = do
    tooltip "Clicking"
    sleep 50
    sendInput "{Click}"
    sleep 50
    sendInput "{Click}"
    sleep 150

main : Promise ()
main = do
  hotkey "F11" $ do
    tooltip "Reloading"
    sleep 500
    reload ()

  hotkey "F12" foo