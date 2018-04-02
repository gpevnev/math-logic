module Expression where

import Parser
import Control.Applicative ((<|>))

data Op = And | Or | Impl
    deriving (Eq, Ord)

instance Show Op where 
    show And  = "&"
    show Or   = "|"
    show Impl = "->"

data Expr =     
    Meta !String
    | Var !String          -- Proposional variable
    | Neg !Expr          -- !Expr
    | Bin Op !Expr !Expr -- Expr `Op` Expr
    deriving (Eq, Ord)

instance Show Expr where
    show = infixForm

-- infix form of Expr
infixForm :: Expr -> String
infixForm (Var s) = s
infixForm (Neg e) = "!" ++ infixForm e ++ ""
infixForm (Bin op e1 e2) = "(" ++ infixForm e1 ++ show op ++ infixForm e2 ++ ")"

-- prefix form of Expr
prefixForm :: Expr -> String
prefixForm (Var s) = s
prefixForm (Neg e) = "(!" ++ prefixForm e ++ ")"
prefixForm (Bin op e1 e2) = "("++ show op ++ "," ++ prefixForm e1 ++ "," ++ prefixForm e2 ++ ")"

expr :: Prs Expr
expr = disj `chainr` (string "->" *> pure (Bin Impl)) where
    disj = conj `chainl` (char '|' *> pure (Bin Or))

    conj = neg `chainl` (char '&' *> pure (Bin And))

    neg = var
      <|> Neg <$> (char '!' *> neg)
      <|> (char '(') *> expr <* (char ')')

    var = Var <$> ((:) <$> upper <*> many (upper <|> digit))
      <|> Meta <$> many1 lower