module Deduction
where

import           Annotation
import           Expression
import           Utility

insertToExpr :: Expr -> [(Expr, Expr)] -> Expr
insertToExpr e m = lookup e m `orElse` case e of
    Bin op e1 e2 -> Bin op (insertToExpr e1 m) (insertToExpr e2 m)
    Neg e1       -> Neg (insertToExpr e1 m)
    _            -> e

ins :: String -> [(String, Expr)] -> Expr
ins e m = insertToExpr (parseExpr e) (map (\(s, e') -> (Var s, e')) m)


deduction :: Proof -> Proof
deduction (Proof (Header hypothesises stmnt) exprs) = Proof resHeader resExprs
  where
    alpha     = last hypothesises
    resHeader = Header (init hypothesises) (Bin Impl alpha stmnt)
    resExprs  = concatMap exprTransform (annotateProof $ Proof resHeader exprs)
    exprTransform (AnnExpr e (Ax         _    )) = axOrHypTransform e
    exprTransform (AnnExpr e (Hypothesis _    )) = axOrHypTransform e
    exprTransform (AnnExpr i (Mp (_, _) (_, j))) = mpTransform i j
    exprTransform (AnnExpr e Wrong) | e == alpha = alphaTransform
                                    | otherwise  = error "oops"

    axOrHypTransform i =
        map (`ins` [("A", alpha), ("I", i)]) ["I", "I->(A->I)", "A->I"]

    mpTransform i j = map
        (`ins` [("A", alpha), ("I", i), ("J", j)])
        ["(A->J)->((A->(J->I))->(A->I))", "(A->(J->I))->(A->I)", "A->I"]
    alphaTransform = map
        (`ins` [("A", alpha)])
        [ "A->(A->A)"
        , "(A->(A->A))->(A->((A->A)->A))->(A->A)"
        , "(A->((A->A)->A))->(A->A)"
        , "(A->((A->A)->A))"
        , "A->A"
        ]




