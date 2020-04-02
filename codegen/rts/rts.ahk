class __drahko
{
  putStr(x)
  {
    MsgBox % x
  }

  crash(msg)
  {
    throw Exception(msg, -1)
  }

  hotkey(hk, fun)
  {
    static funs := {}
    funs[hk] := Func(fun)
    Hotkey, %hk%, Hotkey_Handle
    return
  Hotkey_Handle:
    funs[A_ThisHotkey].("")
    return
  }
}
