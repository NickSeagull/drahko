class __drahko
{
  static funs := {}
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
    this.funs[hk] := fun
    fn := this.funs[hk].bind("","")
    Hotkey, %hk%, %fn%
  }

  _loop(fun)
  {
    loop {
      fun()
    }
  }
}
