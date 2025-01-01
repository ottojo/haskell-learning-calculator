module Lex (Operator (..), Symbol (..), singleSymbolSym, getOp) where

data Operator = Plus | Minus | Mult | Div
  deriving (Show, Eq)

data Symbol = LParenSym | RParenSym | IdentSym String | NumLitSym Double | Operator Operator | LetSym | EqualsSym
  deriving (Show, Eq)

singleSymbolSym :: String -> Symbol
singleSymbolSym str = case str of
  ('(' : _) -> LParenSym
  (')' : _) -> RParenSym
  ('+' : _) -> Operator Plus
  ('-' : _) -> Operator Minus
  ('*' : _) -> Operator Mult
  ('/' : _) -> Operator Div
  ('=' : _) -> EqualsSym
  _ -> error (head str : " is not a valid symbol")

getOp :: Operator -> (Double -> Double -> Double)
getOp Minus = (-)
getOp Plus = (+)
getOp Mult = (*)
getOp Div = (/)
