#Warn

idris_putStr(x)
{
  MsgBox % x
}

idris_TheWorld()
{
  return ""
}

idris_crash(msg)
{
  throw Exception(msg, -1)
}

idris_define_hotkey(hotkeyString, functionName, arg*)
{
  static functionMap := {}, args := {}
  functionMap[hk] := Func(fun), args[hk] := arg
  Hotkey, %hk%, Hotkey_Handle
  return
Hotkey_Handle:
  functionMap[A_ThisHotkey].(args[A_ThisHotkey]*)
  return
}