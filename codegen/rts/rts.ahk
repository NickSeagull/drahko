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
