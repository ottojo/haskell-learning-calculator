module AST (NumericLiteral (..), Identifier (..), Line (..), Assignment (..), Expression (..), Operation (..), literalValue) where

import qualified Lex (Operator (..))

newtype NumericLiteral = NumericLiteral Double
  deriving (Show, Eq)

literalValue :: NumericLiteral -> Double
literalValue v = case v of
  (NumericLiteral x) -> x

newtype Identifier = Identifier String
  deriving (Show, Eq, Ord)

data Line = AssignmentLine Assignment | ExpressionLine Expression
  deriving (Show, Eq)

data Assignment = Assignment
  { assignmentIdent :: Identifier,
    assignmentExpr :: Expression
  }
  deriving (Show, Eq)

data Expression = LiteralExpression NumericLiteral | OperationExpression Operation | IdentifierExpression Identifier
  deriving (Show, Eq)

data Operation = Operation
  { operationLhs :: Expression,
    operationOperand :: Lex.Operator,
    operationRhs :: Expression
  }
  deriving (Show, Eq)
