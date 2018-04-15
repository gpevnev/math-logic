module Annotation where

import           Control.Applicative ((<|>))
import           Data.List           (find)
import           Data.Map.Strict     (Map, empty, fromList, insert, insertWith)
import           Expression          hiding (proof)
import           Parser
import           Prelude             hiding (compare)
import           Utility

-- begin of data

data Annotation =
    Hypothesis !Int
    | Ax !Int
    | Mp !(Int, Expr) !(Int, Expr)
    | Wrong

instance Show Annotation where
    show (Hypothesis n)     = "Предп. " ++ show n
    show (Ax n)             = "Сх. акс. " ++ show n
    show (Mp (i, _) (j, _)) = "M.P. " ++ show i ++ ", " ++ show j
    show Wrong              = "Не доказано"

data AnnotatedExpr = AnnExpr !Expr !Annotation

instance Show AnnotatedExpr where
    show (AnnExpr e a) = infixForm e ++ " (" ++ show a ++ ")"


-- end of data

getAxiom :: Int -> Expr
getAxiom n = fst $ axioms !! (n - 1)

axioms :: [(Expr, Annotation)]
axioms =
        [ (parse expr "A->B->A"                  ,  Ax 1 )
        , (parse expr "(A->B)->(A->B->C)->(A->C)",  Ax 2 )
        , (parse expr "A->B->A&B"                ,  Ax 3 )
        , (parse expr "A&B->A"                   ,  Ax 4 )
        , (parse expr "A&B->B"                   ,  Ax 5 )
        , (parse expr "A->A|B"                   ,  Ax 6 )
        , (parse expr "B->A|B"                   ,  Ax 7 )
        , (parse expr "(A->C)->(B->C)->(A|B->C)" ,  Ax 8 )
        , (parse expr "(A->B)->(A->!B)->!A"      ,  Ax 9 )
        , (parse expr "!!A->A"                   ,  Ax 10)
        ]

annotateProof :: Proof -> [AnnotatedExpr]
annotateProof (Proof (Header hypothesises _) exprs) = proof where

    hypMap = fromList $ zip hypothesises [1..]
    checkHypothesis e = Hypothesis <$> hypMap !? e

    checkAxioms e = snd <$> find (\(e', _) -> eqWithMeta e e') axioms

    eqWithMeta e' a = snd $ compare e' a empty

    compare (Bin op e1 e2) (Bin op' e1' e2') m =
        let (m1, r1) = compare e1 e1' m
            (m2, r2) = compare e2 e2' m1
        in (m2, op == op' && r1 && r2)
    compare (Neg e1)       (Neg e1')         m = compare e1 e1' m
    compare   e1           (Var s)          m =
        case m !? s of
            Nothing           -> (insert s e1 m, True)
            Just expectedExpr -> (m, expectedExpr == e1)
    compare   _               _              m = (m, False)

    linedExprs :: [(Int, Expr)]
    linedExprs = zip [1..] exprs

    statements :: Map Expr Int
    statements = go empty linedExprs where
        go m ((i, e) : es) = go (insertIfAbsent e i m) es
        go m []            = m

    mpRules :: Map Expr Annotation
    mpRules = go empty linedExprs where
        go :: Map Expr Annotation -> [(Int, Expr)] -> Map Expr Annotation
        go m ((i, e@(Bin Impl a b)) : es) = go (
            case statements !? a of
                Just aInd -> insertWith insertF b (Mp (i, e) (aInd, a)) m
                Nothing   -> m) es

        go m (_ : es) = go m es
        go m [] = m

        insertF new@(Mp _ (j', _)) old@(Mp _ (j, _))
            | j' < j = new
            | otherwise = old

        insertF _ _ = undefined

    checkMp curLine e = case mpRules !? e of
        mp@(Just (Mp (i, _) (j, _))) | (i < curLine) && (j < curLine) -> mp
        _                            -> Nothing

    proof = map proofStep linedExprs

    proofStep (curLine, e)=
        let annotation = (checkHypothesis e <|> checkAxioms e <|> checkMp curLine e) `orElse` Wrong
        in AnnExpr e annotation
