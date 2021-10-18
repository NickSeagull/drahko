module Drahko.StackBased where

import Relude hiding (Const)

import Idris.AbsSyntax
import Idris.ElabDecls
import Idris.Main
import Idris.Options

import IRTS.CodegenCommon
import IRTS.Compiler
import IRTS.Lang
import IRTS.Simplified
import Idris.Core.TT

import System.Environment
import System.Exit

import Data.Char
import Data.List
import Debug.Trace

import Data.Hashable

codegenAHKSimple :: CodeGenerator
codegenAHKSimple ci = do
  putTextLn "codegen sdecls"
  let genDecls =
        foldl1 (\x y -> x ++ "\n" ++ y) $
        fmap (\(a, b) -> sdecls2str a b) $ simpleDecls ci
  let declsWithMain = genDecls ++ mainEntry
  putStrLn declsWithMain
  let ofn = outputFile ci
  putStrLn $ "outputFile : " ++ ofn
  writeFile ofn (toText declsWithMain)

cgDecls :: Name -> [Name] -> SExp -> String
cgDecls fname args fbody =
  let (exp, Info _ _ stmts) = runState (cgSExp fbody) (Info "nm1" "nm1" [])
      exp2 = mconcat $ intersperse "\n" $ stmts
   in "function " ++
      jsName fname ++
      "(" ++
      (mconcat $ intersperse "," $ fmap jsName args) ++
      ") " ++ "{\n" ++ exp2 ++ "}\n"
sdecls2str :: Name -> SDecl -> String
sdecls2str fname aaa@(SFun _ fArgs i fBody) = cgDecls fname fArgs fBody

mainEntry = jsName (sMN 0 "runMain") ++ "();" -- main entry!

jsName s =
  "idr_" ++
  let nm = (showCG s)
      nameok c = or [isLetter c, isDigit c]
   in fmap
        (\x ->
           if nameok x
             then x
             else '_')
        nm

data Info = Info
  { newname :: String
  , oldnm :: String
  , jsstmts :: [String]
  }

type Gen a = State Info a

jsStmtConcat ss = mconcat $ intersperse "\n" ss

jsStmtEmpty = ""

var :: Name -> String
var n = "$varglob" ++ jsName n

loc :: Int -> String
loc i = "$local" ++ show i

cgVar :: LVar -> String
cgVar (Loc i) = loc i
cgVar (Glob n) = var n

jsret :: [Char] -> [Char]
jsret s = "return (" ++ s ++ ")"

cgSExp :: SExp -> Gen String
cgSExp (SV (Glob n)) = return $ (jsName n ++ "()")
cgSExp (SV (Loc i)) = return $ (loc i)
cgSExp (SApp _ fname args) =
  return $
  (jsName fname) ++
  "(" ++ (mconcat $ intersperse "," $ fmap cgVar args) ++ ");\n"
cgSExp (SLet _ valE bodyE) = do
  val <- cgSExp valE
  modify
    (\(Info newnm oldnm stmts) ->
       Info ((++) "v" $ take 6 $ show $ abs $ hash $ newnm ++ "1") newnm $
       stmts ++ [" var " ++ newnm ++ "=" ++ val])
  cgSExp bodyE
cgSExp (SOp primfn lvars) = do
  Info nm old st <- get
  return $ cgPrimFn primfn $ ["", old]
cgSExp (SConst x) = return $ cgConst x
cgSExp SNothing = return $ "None"
cgSExp (SCon Nothing x2 x3 x4) = return $ "con None"
cgSExp _ = return jsStmtEmpty

cgConst :: Const -> String
cgConst (I i) = show i
cgConst (Ch i) = show i -- Treat Char as ints, because PHP treats them as Strings...
cgConst (BI i) = show i
cgConst (Str s) = show s
cgConst TheWorld = "0"
cgConst x
  | isTypeConst x = "0"
cgConst x = error $ "Constant " <> show x <> " not compilable yet"

cgPrimFn :: PrimFn -> [String] -> String
cgPrimFn (LPlus (ATInt _)) [l, r] = "(" ++ l ++ " + " ++ r ++ ")"
cgPrimFn (LMinus (ATInt _)) [l, r] = "(" ++ l ++ " - " ++ r ++ ")"
cgPrimFn (LTimes (ATInt _)) [l, r] = "(" ++ l ++ " * " ++ r ++ ")"
cgPrimFn (LEq (ATInt _)) [l, r] = "(" ++ l ++ " == " ++ r ++ ")"
cgPrimFn (LSLt (ATInt _)) [l, r] = "(" ++ l ++ " < " ++ r ++ ")"
cgPrimFn (LSLe (ATInt _)) [l, r] = "(" ++ l ++ " <= " ++ r ++ ")"
cgPrimFn (LSGt (ATInt _)) [l, r] = "(" ++ l ++ " > " ++ r ++ ")"
cgPrimFn (LSGe (ATInt _)) [l, r] = "(" ++ l ++ " >= " ++ r ++ ")"
cgPrimFn LStrEq [l, r] = "(" ++ l ++ " == " ++ r ++ ")"
cgPrimFn LStrRev [x] = "strrev(" ++ x ++ ")"
cgPrimFn LStrLen [x] = "strlen(utf8_decode(" ++ x ++ "))"
cgPrimFn LStrHead [x] = "ord(" ++ x ++ "[0])"
cgPrimFn LStrIndex [x, y] = "ord(" ++ x ++ "[" ++ y ++ "])"
cgPrimFn LStrTail [x] = "substr(" ++ x ++ ", 1)"
cgPrimFn (LIntStr _) [x] = "\"" ++ x ++ "\""
cgPrimFn (LChInt _) [x] = x
cgPrimFn (LIntCh _) [x] = x
cgPrimFn (LSExt _ _) [x] = x
cgPrimFn (LTrunc _ _) [x] = x
cgPrimFn LWriteStr [_, str] = "console.log(" ++ str ++ ")"
cgPrimFn LReadStr [_] = "idris_readStr()"
cgPrimFn LStrConcat [l, r] = "idris_append(" ++ l ++ ", " ++ r ++ ")"
cgPrimFn LStrCons [l, r] = "idris_append(chr(" ++ l ++ "), " ++ r ++ ")"
cgPrimFn (LStrInt _) [x] = x
cgPrimFn op exps = "error(\"OPERATOR " ++ show op ++ " NOT IMPLEMENTED!!!!\")"