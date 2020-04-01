module Drahko.FFI

%default total
%access public export

mutual
  data AutoHotkeyRaw : Type -> Type where
    MkAutoHotkeyRaw : (x:t) -> AutoHotkeyRaw t
  -- %used MkAutoHotkeyRaw x

  data AHK_FnTypes : Type -> Type where
    AHK_Fn : AHK_Types s -> AHK_FnTypes t -> AHK_FnTypes (s -> t)
    AHK_FnIO : AHK_Types s -> AHK_Types t -> AHK_FnTypes (s -> IO' l t)
    AHK_FnBase : AHK_Types s -> AHK_Types t -> AHK_FnTypes (s -> t)

  data AHK_Types : Type -> Type where
    AHK_Str    : AHK_Types String
    AHK_Int    : AHK_Types Int
    AHK_Float  : AHK_Types Double
    AHK_Bool   : AHK_Types Bool
    AHK_Unit   : AHK_Types ()
    AHK_Raw    : AHK_Types (Raw a)
    AHK_Ptr    : AHK_Types Ptr
    AHK_FnT    : AHK_FnTypes t -> AHK_Types t

  data AHK_Foreign
    = AHK_DotAccess String String
    | AHK_Command String
    | AHK_Function String

  %error_reverse
  FFI_AHK : FFI
  FFI_AHK = MkFFI AHK_Types AHK_Foreign String

  %error_reverse
  Promise : Type -> Type
  Promise = IO' FFI_AHK

  IO : Type -> Type
  IO a = IO' FFI_AHK a

mutual
  -- Translates an AHK-conventions function to an Idris-conventions
  -- one by inserting an additional dummy 'world' argument.
  fromAHKFn : AHK_FnTypes a -> a -> a
  fromAHKFn (AHK_Fn s t)     f = \x => fromAHKFn t (f (toAHK s x))
  fromAHKFn (AHK_FnIO s t)   f = \x => pure (fromAHK t (believe_me (f (toAHK s x))))
  fromAHKFn (AHK_FnBase s t) f = \x => fromAHK t (f (toAHK s x))

  toAHKFn : AHK_FnTypes a -> a -> a
  toAHKFn (AHK_Fn s t)     f = \x => toAHKFn t (f (fromAHK s x))
  toAHKFn (AHK_FnIO s t)   f = \x => believe_me (toAHK t (unsafePerformIO (f (fromAHK s x))))
  toAHKFn (AHK_FnBase s t) f = \x => toAHK t (f (fromAHK s x))

  fromAHK : AHK_Types a -> a -> a
  fromAHK AHK_Str        s = s
  fromAHK AHK_Int        i = i
  fromAHK AHK_Float      f = f
  fromAHK AHK_Bool       b = b
  fromAHK AHK_Unit       u = u
  fromAHK AHK_Raw        a = a
  fromAHK AHK_Ptr        p = p
  fromAHK (AHK_FnT t)    f = fromAHKFn t f
  -- fromAHK (AHK_Pair s t) p = (fromAHK s (fst p), fromAHK t (snd p))
  -- fromAHK (AHK_List s)   l = map (fromAHK s) l

  toAHK : AHK_Types a -> a -> a
  toAHK AHK_Str        s = s
  toAHK AHK_Int        i = i
  toAHK AHK_Float      f = f
  toAHK AHK_Bool       b = b
  toAHK AHK_Unit       u = u
  toAHK AHK_Raw        a = a
  toAHK AHK_Ptr        p = p
  toAHK (AHK_FnT t)    f = toAHKFn t f
  -- toAHK (AHK_Pair s t) p = (toAHK s (fst p), toAHK t (snd p))
  -- toAHK (AHK_List s)   l = map (toAHK s) l

%inline
fromAHKFTy : FTy FFI_AHK xs ty -> ty -> ty
fromAHKFTy (FRet t)   f = do x <- f; pure (fromAHK t x)
fromAHKFTy (FFun s t) f = \x => fromAHKFTy t (f (toAHK s x))

||| Call an AutoHotkey function.
%inline
call_function : String -> (ty : Type) -> {auto fty : FTy FFI_AHK [] ty} -> ty
call_function name ty {fty} =
  fromAHKFTy fty (foreign FFI_AHK (AHK_Function name) ty)

||| Call an AutoHotkey command.
%inline
call_command : String -> (ty : Type) -> {auto fty : FTy FFI_AHK [] ty} -> ty
call_command name =
  foreign FFI_AHK (AHK_Command name)