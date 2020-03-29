class __drahko
{
  putStr(x)
  {
    MsgBox % x
  }

  TheWorld()
  {
    return ""
  }

  crash(msg)
  {
    throw Exception(msg, -1)
  }

  showreg(name, register)
  {
    MsgBox, % "REGISTER ----- " . name . "`n" . this.regstr(register)
  }

  regstr(register)
  {
    if (register.Length())
    {
      For k, v in register {
        msg .= "[" . k . "] => `n`t" . this.regstr(v) . "`n"
      }
      return msg
    } else {
      return register
    }
  }
}
