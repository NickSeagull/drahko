module IdrisCG.AutoHotkey.Syntax where

import Relude

newtype Name = Name {unName :: Text}
  deriving (Eq, Show, IsString, Generic, Ord)

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
  | Projection Expression Expression
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
  | NoOp
  | SubroutineCall Name [Expression]
  | Command Name [Expression]
  deriving (Eq, Show)

newtype Program = Program [Statement]
  deriving (Eq, Show)
