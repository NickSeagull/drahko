-- | AutoHotkey AST definition, this is mostly taken from
-- `idris-vimscript`'s VimScript AST definition:
-- https://github.com/owickstrom/idris-vimscript/blob/master/codegen/src/Vimscript/AST.hs
module AutoHotkey.Syntax where

import Relude
import Text.Encoding.Z (zEncodeString)

newtype Name = Name {unName :: Text}
  deriving (Eq, Show, IsString, Generic, Ord)

name :: String -> Name
name = Name . toText . zEncodeString

data BinaryOperator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Equal
  | LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual
  | Concat
  deriving (Eq, Show)

data Literal
  = Integer Integer
  | Floating Double
  | String Text
  | List [Expression]
  deriving (Eq, Show)

data Expression
  = BinaryOperatorApply BinaryOperator Expression Expression
  | Literal Literal
  | Variable Name
  | Apply Expression [Expression]
  deriving (Eq, Show)

type Block = [Statement]

data ConditionalCase = ConditionalCase Expression Block
  deriving (Eq, Show)

data ConditionalStatement
  = ConditionalStatement ConditionalCase [ConditionalCase] (Maybe Block)
  deriving (Eq, Show)

data Statement
  = Let Name Expression
  | Return Expression
  | While Expression Block
  | Break
  | Continue
  | Function Name [Name] Block
  | Call Name [Expression]
  | Condition ConditionalStatement
  | Assignment Name Expression
  | SubroutineCall Name [Expression]
  deriving (Eq, Show)

newtype Program = Program [Statement]
  deriving (Eq, Show)
