module Expression where

import Parser
import Control.Applicative ((<|>))

data Op = And | Or | Impl
    deriving Eq

instance Show Op where 
    show And  = "&"
    show Or   = "|"
    show Impl = "->"

data Expr = 
    Meta !String     -- Metavar 
    | Var !String      -- Proposional variable
    | Neg !Expr        -- !Expr
    | Bin Op !Expr !Expr  
    deriving Eq

instance Show Expr where
    show (Meta s) = s
    show (Var s) = s
    show (Neg e) = "(!" ++ show e ++ ")"
    show (Bin op e1 e2) = "("++ show op ++ "," ++ show e1 ++ "," ++ show e2 ++ ")"

data Header = Header ![Expr] !Expr
    deriving Show

data File = File !Header ![Expr]
    deriving Show

file :: Prs File
file = File <$> header <* (char '\n') <*> expr `endBy` newLine

header :: Prs Header
header = Header <$> (expr `sepBy` char ',') <* string "|-" <*> expr

line :: Prs Expr
line = expr <* newLine

expr :: Prs Expr
expr = disj `chainr` (string "->" *> pure (Bin Impl)) where
    disj :: Prs Expr
    disj = conj `chainl` (char '|' *> pure (Bin Or))

    conj :: Prs Expr 
    conj = neg `chainl` (char '&' *> pure (Bin And))

    neg :: Prs Expr
    neg = Meta <$> many1 lower
      <|> Var <$> ((:) <$> upper <*> many (upper <|> digit))
      <|> Neg <$> (char '!' *> neg)
      <|> (char '(') *> expr <* (char ')')
