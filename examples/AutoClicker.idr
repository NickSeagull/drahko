import Drahko

main : Promise ()
main = do
  hotkey "F11" $ do
    tooltip "Reloading"
    sleep 500
    reload ()

  hotkey "F12" $ do
    tooltip "Clicking"
    sleep 50
    sendInput "{Click}"
    sleep 50
    sendInput "{Click}"
    sleep 150