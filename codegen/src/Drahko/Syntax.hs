module Drahko.Syntax where

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
  | DotAccess Expression Expression
  deriving (Eq, Show)

type Block = [Statement]

data ConditionalCase = ConditionalCase Expression Block
  deriving (Eq, Show)

data ConditionalStatement
  = ConditionalStatement ConditionalCase [ConditionalCase] (Maybe Block)
  deriving (Eq, Show)

data Statement
  = Return Expression
  | While Expression Block
  | Break
  | Continue
  | Function Name [Name] Block
  | Call Expression [Expression]
  | Condition ConditionalStatement
  | Assignment Expression Expression
  | NoOp
  | RawExpression Expression
  | Command Name [Expression]
  | Class Name (Maybe Name) [Statement]
  deriving (Eq, Show)

newtype Program = Program [Statement]
  deriving (Eq, Show)
