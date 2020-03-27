#Warn

idris_putStr(x)
{
  MsgBox % x
}

TheWorld()
{
  return ""
}

idris_crash(msg)
{
  throw Exception(msg, -1)
}
