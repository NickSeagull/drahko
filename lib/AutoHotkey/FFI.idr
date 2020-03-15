module AutoHotkey.FFI

%default total

mutual
  public export
  data AutoHotkeyFn t = MkAutoHotkeyFn t

  public export
  data AHK_FnTypes : Type -> Type where
    AHK_Fn : AHK_Types s -> AHK_FnTypes t -> AHK_FnTypes (s -> t)
    AHK_FnIO : AHK_Types t -> AHK_FnTypes (IO' l t)
    AHK_FnBase : AHK_Types t -> AHK_FnTypes t

  public export
  data AHK_Types : Type -> Type where
    AHK_Str    : AHK_Types String
    AHK_Int    : AHK_Types Int
    AHK_Float  : AHK_Types Double
    AHK_Unit   : AHK_Types ()
    AHK_Raw    : AHK_Types (Raw a)
    AHK_FnT    : AHK_FnTypes t -> AHK_Types (AutoHotkeyFn t)

mutual
  public export
  data AHK_Foreign
    = AHK_DotAccess String String
    | AHK_Function String

  %error_reverse
  public export
  FFI_AHK : FFI
  FFI_AHK = MkFFI AHK_Types AHK_Foreign String

  %error_reverse
  public export
  AHK_IO : Type -> Type
  AHK_IO = IO' FFI_AHK

  IO : Type -> Type
  IO a = IO' FFI_AHK a