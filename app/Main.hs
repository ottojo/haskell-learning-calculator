{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception (assert)
import Data.List (elemIndex)
import Data.Text (pack, strip, unpack)
import Data.Tuple.HT (mapFst)
import GHC.Arr ((!))
import Text.Regex.Base (makeRegex, matchOnceText)
import Text.Regex.PCRE (Regex)

newtype NumericLiteralAST = MkNumericLiteral Double
  deriving (Show, Eq)

newtype IdentifierAST = MkIdentifier String
  deriving (Show, Eq)

data OperatorT = Plus | Minus | Mult | Div
  deriving (Show, Eq)

data Symbol = LParenSym | RParenSym | IdentSym String | NumLitSym Double | OperatorSym OperatorT | LetSym | EqualsSym
  deriving (Show, Eq)

data LineAST = AssignmentLine AssignmentAST | ExpressionLine ExpressionAST
  deriving (Show, Eq)

data AssignmentAST = MkAssignment
  { assignmentIdent :: IdentifierAST,
    assignmentExpr :: ExpressionAST
  }
  deriving (Show, Eq)

data ExpressionAST = LiteralExpression NumericLiteralAST | OperationExpression OperationAST | IdentifierExpression IdentifierAST
  deriving (Show, Eq)

data OperationAST = MkOperation
  { operationLhs :: ExpressionAST,
    operationOperand :: OperatorT,
    operationRhs :: ExpressionAST
  }
  deriving (Show, Eq)

parseLine :: [Symbol] -> (LineAST, [Symbol])
parseLine line = case line of
  (LetSym : rest) -> mapFst AssignmentLine (parseAssignment rest)
  _ -> mapFst ExpressionLine (parseExpression line)

parseAssignment :: [Symbol] -> (AssignmentAST, [Symbol])
parseAssignment tokens =
  case tokens of
    (IdentSym identifier : rhsTokens) ->
      ( case rhsTokens of
          (EqualsSym : expressionTokens) ->
            let (expression, rest) = parseExpression expressionTokens
             in (MkAssignment {assignmentIdent = MkIdentifier identifier, assignmentExpr = expression}, rest)
          _ -> error "Expected ="
      )
    _ -> error "Expected identifier"

parseExpression :: [Symbol] -> (ExpressionAST, [Symbol])
parseExpression line = case line of
  (LParenSym : rest) ->
    ( case parseOperation rest of
        (op, RParenSym : r_rest) -> (OperationExpression op, r_rest)
        (_, []) -> error "expected RParen, got nothing"
        (_, e : _) -> error ("expected RParen, got " ++ show e)
    )
  (IdentSym identifier : rest) -> (IdentifierExpression (MkIdentifier identifier), rest)
  (NumLitSym literal : rest) -> (LiteralExpression (MkNumericLiteral literal), rest)
  _ -> error ("Unexpected symbol in expression: " ++ show (head line))

parseLiteral :: String -> Double
parseLiteral line
  | head line == '-' = (-1) * parseLiteral (tail line)
  | head line == '+' = parseLiteral (tail line)
  | otherwise = parseUnsignedLiteral line

parseOperation :: [Symbol] -> (OperationAST, [Symbol])
parseOperation tokens =
  let (lhsExpr, midAndRhs) = parseExpression tokens
   in case midAndRhs of
        (OperatorSym op : rhsTokens) ->
          let (rhsExpr, rest) = parseExpression rhsTokens
           in (MkOperation {operationLhs = lhsExpr, operationOperand = op, operationRhs = rhsExpr}, rest)
        _ -> error "Expected operator"

parseInteger :: String -> Integer
parseInteger line = read line :: Integer

parseUnsignedLiteral :: String -> Double
parseUnsignedLiteral line =
  let maybeIndex = elemIndex '.' line
   in case maybeIndex of
        Just decimalSepIndex ->
          let wholePart = take decimalSepIndex line
              fractionalPart = drop (decimalSepIndex + 1) line
           in fromIntegral (parseInteger wholePart) + 10 ^^ ((-1) * length fractionalPart) * fromIntegral (parseInteger fractionalPart)
        Nothing -> fromIntegral (parseInteger line)

singleSymbolSym :: String -> Symbol
singleSymbolSym str = case str of
  ('(' : _) -> LParenSym
  (')' : _) -> RParenSym
  ('+' : _) -> OperatorSym Plus
  ('-' : _) -> OperatorSym Minus
  ('*' : _) -> OperatorSym Mult
  ('/' : _) -> OperatorSym Div
  ('=' : _) -> EqualsSym
  _ -> error (head str : " is not a valid symbol")

stripString :: String -> String
stripString = unpack . strip . pack

calculatorLex :: String -> [Symbol]
calculatorLex = go []
  where
    go :: [Symbol] -> String -> [Symbol]
    go result remaining
      | null remaining = result
      | head remaining == ' ' = go result (tail remaining)
      | otherwise =
          let letKeywordRegex :: Regex = makeRegex "^\\s*let\\s"
              numLitRegex :: Regex = makeRegex "^\\s*([+\\-])?\\d+(\\.(\\d+))?"
              singleSymbolRegex :: Regex = makeRegex "^\\s*([+*\\/\\(\\)=\\-])"
              identifierRegex :: Regex = makeRegex "^\\s*([a-z]+)"
           in case matchOnceText letKeywordRegex remaining of
                (Just (_before, _matches, after)) -> go (result ++ [LetSym]) after
                Nothing -> case matchOnceText numLitRegex remaining of
                  (Just (_before, matches, after)) -> go (result ++ [NumLitSym (parseLiteral (stripString (fst (matches ! 0))))]) after
                  Nothing -> case matchOnceText singleSymbolRegex remaining of
                    (Just (_before, matches, after)) -> go (result ++ [singleSymbolSym (fst (matches ! 1))]) after
                    Nothing -> case matchOnceText identifierRegex remaining of
                      (Just (_before, matches, after)) -> go (result ++ [IdentSym (fst (matches ! 1))]) after
                      Nothing -> error "todo: other regexes"

main :: IO ()
main = do
  print (assert (parseInteger "123" == 123) "Parsing integer 123 works!")
  print (assert (parseInteger "0" == 0) "Parsing integer 0 works!")
  print (assert (parseLiteral "123" == 123) "Parsing literal 123 works!")
  print (assert (parseLiteral "-123" == -123) "Parsing literal -123 works!")
  print (assert (parseLiteral "123.123" == 123.123) "Parsing literal 123.123 works!")
  print (assert (parseLiteral "-123.456" == -123.456) "Parsing literal -123.456 works!")
  print (assert (parseLiteral "0.0" == 0.0) "Parsing literal 0.0 works!")
  print (assert (calculatorLex "   let " == [LetSym]) "Lexing let works!")
  print (assert (calculatorLex "  123 " == [NumLitSym 123]) "Lexing integer works!")
  print (assert (calculatorLex "  123.4 " == [NumLitSym 123.4]) "Lexing float works!")
  print (assert (calculatorLex "  -123.4 " == [NumLitSym (-123.4)]) "Lexing negative float works!")
  print (assert (calculatorLex "  +123.4 " == [NumLitSym 123.4]) "Lexing positive float works!")
  print (assert (calculatorLex "  10 let " == [NumLitSym 10, LetSym]) "Lexing integer with clutter works!")
  print (assert (calculatorLex "  72)" == [NumLitSym 72, RParenSym]) "Lexing integer before RParen works!")
  print (assert (calculatorLex "  +123.4 -10" == [NumLitSym 123.4, NumLitSym (-10)]) "Lexing positive float with clutter works!")
  print (assert (calculatorLex "  +123.4 - -10" == [NumLitSym 123.4, OperatorSym Minus, NumLitSym (-10)]) "Lexing operation works!")
  print (assert (calculatorLex "(+123.4 - -10)" == [LParenSym, NumLitSym 123.4, OperatorSym Minus, NumLitSym (-10), RParenSym]) "Lexing bracketed operation works!")
  print (assert (parseLine (calculatorLex "(+123.4 - -10)") == (ExpressionLine (OperationExpression (MkOperation {operationLhs = LiteralExpression (MkNumericLiteral 123.4), operationOperand = Minus, operationRhs = LiteralExpression (MkNumericLiteral (-10.0))})), [])) "Parsing Operation works!")
  print (assert (parseLine (calculatorLex "let pi = 3.1415926") == (AssignmentLine (MkAssignment {assignmentIdent = MkIdentifier "pi", assignmentExpr = LiteralExpression (MkNumericLiteral 3.1415926)}), [])) "Parsing assignment works!")

  let numLitRegex :: Regex = makeRegex "^\\s*([+\\-])?\\d+(\\.(\\d+))?"
   in print (matchOnceText numLitRegex "  10 sdf8934&/2")

  print (parseLine (calculatorLex "(+123.4 - -10)"))
  print "Done"
