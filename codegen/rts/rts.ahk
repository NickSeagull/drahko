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
}
