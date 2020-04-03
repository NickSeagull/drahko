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
    fn := fun.bind("","")
    Hotkey, %hk%, %fn%
  }
}
