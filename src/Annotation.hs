module Annotation where

import Prelude hiding (compare)
import Expression
import Control.Applicative ((<|>))
import Parser
import Utility
import Data.List (find)
import System.IO
import Data.Map.Strict (Map, insert, insertWith, empty, fromList)

-- IO part

data Header = Header ![Expr] !Expr
    deriving Show

data File = File !Header ![Expr]
    deriving Show

header :: Prs Header
header = Header <$> (expr `sepBy` char ',') <* string "|-" <*> expr

file :: Prs File
file = File <$> header <* newLine
            <*> expr `sepBy` newLine

-- end of IO part

-- begin of data

data Annotation = 
    Hypothesis !Int
    | Ax !Int
    | Mp !Int !Int
    | Wrong

instance Show Annotation where
    show (Hypothesis n) = "Предп. " ++ show n
    show (Ax n)         = "Сх. акс. " ++ show n
    show (Mp i j)       = "M.P. " ++ show i ++ ", " ++ show j
    show (Wrong)        = "Не доказано"

data AnnotatedExpr = AnnExpr Int Expr Annotation

instance Show AnnotatedExpr where
    show (AnnExpr n e a) = "(" ++ show n ++ ") " ++ infixForm e ++ " (" ++ show a ++ ")"

-- end of data

annotateProof :: File -> Handle -> IO ()
annotateProof (File (Header hypothesises _) exprs) handle = proof where
    
    hypMap = fromList $ zip hypothesises [1..] 
    checkHypothesis e = Hypothesis <$> hypMap !? e 

    checkAxioms e = snd <$> find (\(e', _) -> eqWithMeta e e') axioms

    eqWithMeta e' a = snd $ compare e' a empty

    compare (Bin op e1 e2) (Bin op' e1' e2') m = 
        let (m1, r1) = compare e1 e1' m
            (m2, r2) = compare e2 e2' m1 
        in (m2, op == op' && r1 && r2)
    compare (Neg e1)       (Neg e1')         m = compare e1 e1' m 
    compare (Var s1)       (Var s1')         m = (m, s1 == s1')
    compare   e1           (Meta s)          m = 
        case m !? s of 
            Nothing -> (insert s e1 m, True)
            Just expectedExpr -> (m, expectedExpr == e1) 
    compare   _               _              m = (m, False)

    axioms = [ (parse expr "a->b->a"                  ,  Ax 1 )
             , (parse expr "(a->b)->(a->b->c)->(a->c)",  Ax 2 )
             , (parse expr "a->b->a&b"                ,  Ax 3 )
             , (parse expr "a&b->a"                   ,  Ax 4 )
             , (parse expr "a&b->b"                   ,  Ax 5 )
             , (parse expr "a->a|b"                   ,  Ax 6 )
             , (parse expr "b->a|b"                   ,  Ax 7 )
             , (parse expr "(a->c)->(b->c)->(a|b->c)" ,  Ax 8 )
             , (parse expr "(a->b)->(a->!b)->!a"      ,  Ax 9 )
             , (parse expr "!!a->a"                   ,  Ax 10)
             ]

    linedExprs :: [(Int, Expr)]
    linedExprs = zip [1..] exprs

    statements :: Map Expr Int
    statements = go empty linedExprs where 
        go m ((i, e) : es) = go (insertIfAbsent e i m) es
        go m [] = m
    
    mpRules :: Map Expr Annotation
    mpRules = go empty linedExprs where
        go :: Map Expr Annotation -> [(Int, Expr)] -> Map Expr Annotation
        go m ((i, (Bin Impl a b)) : es) = go (
            case statements !? a of
                Just aInd -> insertWith insertF b (Mp i aInd) m
                Nothing -> m) es

        go m (_ : es) = go m es
        go m [] = m

        insertF new@(Mp _ j') old@(Mp _ j) 
            | j' < j = new
            | otherwise = old

        insertF _ _ = undefined

    checkMp curLine e = case mpRules !? e of 
        mp@(Just (Mp i j)) | (i < curLine) && (j < curLine) -> mp
        _                                                   -> Nothing

    proof = mapM_ proofStep linedExprs

    proofStep (curLine, e)= 
        let annotation = (checkHypothesis e <|> checkAxioms e <|> checkMp curLine e) `orElse` Wrong
        in hPutStrLn handle (show (AnnExpr curLine e annotation))