{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified AST
import Control.Exception (assert)
import Data.List (elemIndex)
import qualified Data.Map as Map
import Data.Text (pack, strip, unpack)
import Data.Tuple.HT (mapFst)
import GHC.Arr ((!))
import qualified Lex (Operator (..), Symbol (..), singleSymbolSym, getOp)
import Text.Regex.Base (makeRegex, matchOnceText)
import Text.Regex.PCRE (Regex)

parseLine :: [Lex.Symbol] -> (AST.Line, [Lex.Symbol])
parseLine line = case line of
  (Lex.LetSym : rest) -> mapFst AST.AssignmentLine (parseAssignment rest)
  _ -> mapFst AST.ExpressionLine (parseExpression line)

parseAssignment :: [Lex.Symbol] -> (AST.Assignment, [Lex.Symbol])
parseAssignment tokens =
  case tokens of
    (Lex.IdentSym identifier : rhsTokens) ->
      ( case rhsTokens of
          (Lex.EqualsSym : expressionTokens) ->
            let (expression, rest) = parseExpression expressionTokens
             in (AST.Assignment {AST.assignmentIdent = AST.Identifier identifier, AST.assignmentExpr = expression}, rest)
          _ -> error "Expected ="
      )
    _ -> error "Expected identifier"

parseExpression :: [Lex.Symbol] -> (AST.Expression, [Lex.Symbol])
parseExpression line = case line of
  (Lex.LParenSym : rest) ->
    ( case parseOperation rest of
        (op, Lex.RParenSym : r_rest) -> (AST.OperationExpression op, r_rest)
        (_, []) -> error "expected RParen, got nothing"
        (_, e : _) -> error ("expected RParen, got " ++ show e)
    )
  (Lex.IdentSym identifier : rest) -> (AST.IdentifierExpression (AST.Identifier identifier), rest)
  (Lex.NumLitSym literal : rest) -> (AST.LiteralExpression (AST.NumericLiteral literal), rest)
  _ -> error ("Unexpected symbol in expression: " ++ show (head line))

parseLiteral :: String -> Double
parseLiteral line
  | head line == '-' = (-1) * parseLiteral (tail line)
  | head line == '+' = parseLiteral (tail line)
  | otherwise = parseUnsignedLiteral line

parseOperation :: [Lex.Symbol] -> (AST.Operation, [Lex.Symbol])
parseOperation tokens =
  let (lhsExpr, midAndRhs) = parseExpression tokens
   in case midAndRhs of
        (Lex.Operator op : rhsTokens) ->
          let (rhsExpr, rest) = parseExpression rhsTokens
           in (AST.Operation {AST.operationLhs = lhsExpr, AST.operationOperand = op, AST.operationRhs = rhsExpr}, rest)
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

stripString :: String -> String
stripString = unpack . strip . pack

calculatorLex :: String -> [Lex.Symbol]
calculatorLex = go []
  where
    go :: [Lex.Symbol] -> String -> [Lex.Symbol]
    go result remaining
      | null remaining = result
      | head remaining == ' ' = go result (tail remaining)
      | otherwise =
          let letKeywordRegex :: Regex = makeRegex "^\\s*let\\s"
              numLitRegex :: Regex = makeRegex "^\\s*([+\\-])?\\d+(\\.(\\d+))?"
              singleSymbolRegex :: Regex = makeRegex "^\\s*([+*\\/\\(\\)=\\-])"
              identifierRegex :: Regex = makeRegex "^\\s*([a-z]+)"
           in case matchOnceText letKeywordRegex remaining of
                (Just (_before, _matches, after)) -> go (result ++ [Lex.LetSym]) after
                Nothing -> case matchOnceText numLitRegex remaining of
                  (Just (_before, matches, after)) -> go (result ++ [Lex.NumLitSym (parseLiteral (stripString (fst (matches ! 0))))]) after
                  Nothing -> case matchOnceText singleSymbolRegex remaining of
                    (Just (_before, matches, after)) -> go (result ++ [Lex.singleSymbolSym (fst (matches ! 1))]) after
                    Nothing -> case matchOnceText identifierRegex remaining of
                      (Just (_before, matches, after)) -> go (result ++ [Lex.IdentSym (fst (matches ! 1))]) after
                      Nothing -> error ("todo: other regexes! Could not lex next token in: " ++ remaining)



eval :: AST.Expression -> Map.Map AST.Identifier AST.Expression -> Double
eval expr table = case expr of
  (AST.LiteralExpression lit) -> AST.literalValue lit
  (AST.IdentifierExpression ident) -> case Map.lookup ident table of
    (Just res) -> eval res table
    Nothing -> error ("No entry for " ++ show ident)
  (AST.OperationExpression op) -> Lex.getOp (AST.operationOperand op) (eval (AST.operationLhs op) table) (eval (AST.operationRhs op) table)

repl :: Map.Map AST.Identifier AST.Expression -> IO ()
repl table =
  do
    lineStr <- getLine
    let newLine = parseLine (calculatorLex lineStr)
     in do
          case newLine of
            (AST.ExpressionLine exprLine, []) -> do
              print exprLine
              print (eval exprLine table)
              repl table
            (AST.AssignmentLine assignLine, []) -> do
              print assignLine
              let new_table = Map.insert (AST.assignmentIdent assignLine) (AST.assignmentExpr assignLine) table
               in repl new_table
            (_, first : rest) -> do
              print ("Could not fully parse line. Unparsed symbols: " ++ show (first : rest))
              repl table

main :: IO ()
main = do
  print (assert (parseInteger "123" == 123) "Parsing integer 123 works!")
  print (assert (parseInteger "0" == 0) "Parsing integer 0 works!")
  print (assert (parseLiteral "123" == 123) "Parsing literal 123 works!")
  print (assert (parseLiteral "-123" == -123) "Parsing literal -123 works!")
  print (assert (parseLiteral "123.123" == 123.123) "Parsing literal 123.123 works!")
  print (assert (parseLiteral "-123.456" == -123.456) "Parsing literal -123.456 works!")
  print (assert (parseLiteral "0.0" == 0.0) "Parsing literal 0.0 works!")
  print (assert (calculatorLex "   let " == [Lex.LetSym]) "Lexing let works!")
  print (assert (calculatorLex "  123 " == [Lex.NumLitSym 123]) "Lexing integer works!")
  print (assert (calculatorLex "  123.4 " == [Lex.NumLitSym 123.4]) "Lexing float works!")
  print (assert (calculatorLex "  -123.4 " == [Lex.NumLitSym (-123.4)]) "Lexing negative float works!")
  print (assert (calculatorLex "  +123.4 " == [Lex.NumLitSym 123.4]) "Lexing positive float works!")
  print (assert (calculatorLex "  10 let " == [Lex.NumLitSym 10, Lex.LetSym]) "Lexing integer with clutter works!")
  print (assert (calculatorLex "  72)" == [Lex.NumLitSym 72, Lex.RParenSym]) "Lexing integer before RParen works!")
  print (assert (calculatorLex "  +123.4 -10" == [Lex.NumLitSym 123.4, Lex.NumLitSym (-10)]) "Lexing positive float with clutter works!")
  print (assert (calculatorLex "  +123.4 - -10" == [Lex.NumLitSym 123.4, Lex.Operator Lex.Minus, Lex.NumLitSym (-10)]) "Lexing operation works!")
  print (assert (calculatorLex "(+123.4 - -10)" == [Lex.LParenSym, Lex.NumLitSym 123.4, Lex.Operator Lex.Minus, Lex.NumLitSym (-10), Lex.RParenSym]) "Lexing bracketed operation works!")
  print
    ( assert
        ( parseLine (calculatorLex "(+123.4 - -10)")
            == ( AST.ExpressionLine
                   ( AST.OperationExpression
                       ( AST.Operation
                           { AST.operationLhs = AST.LiteralExpression (AST.NumericLiteral 123.4),
                             AST.operationOperand = Lex.Minus,
                             AST.operationRhs = AST.LiteralExpression (AST.NumericLiteral (-10.0))
                           }
                       )
                   ),
                 []
               )
        )
        "Parsing Operation works!"
    )

  print
    ( assert
        ( parseLine (calculatorLex "let pi = 3.1415926")
            == ( AST.AssignmentLine
                   ( AST.Assignment
                       { AST.assignmentIdent = AST.Identifier "pi",
                         AST.assignmentExpr = AST.LiteralExpression (AST.NumericLiteral 3.1415926)
                       }
                   ),
                 []
               )
        )
        "Parsing assignment works!"
    )

  print
    ( assert
        ( parseLine (calculatorLex "let a = ( pi * ( r * r ) )")
            == ( AST.AssignmentLine
                   ( AST.Assignment
                       { AST.assignmentIdent = AST.Identifier "a",
                         AST.assignmentExpr =
                           AST.OperationExpression
                             ( AST.Operation
                                 { AST.operationLhs = AST.IdentifierExpression (AST.Identifier "pi"),
                                   AST.operationOperand = Lex.Mult,
                                   AST.operationRhs =
                                     AST.OperationExpression
                                       ( AST.Operation
                                           { AST.operationLhs = AST.IdentifierExpression (AST.Identifier "r"),
                                             AST.operationOperand = Lex.Mult,
                                             AST.operationRhs = AST.IdentifierExpression (AST.Identifier "r")
                                           }
                                       )
                                 }
                             )
                       }
                   ),
                 []
               )
        )
        "Parsing assignment with nested expression works!"
    )

  let numLitRegex :: Regex = makeRegex "^\\s*([+\\-])?\\d+(\\.(\\d+))?"
   in print (matchOnceText numLitRegex "  10 sdf8934&/2")

  print (parseLine (calculatorLex "(+123.4 - -10)"))
  print "Done"

  repl Map.empty
