module Lib
    ( task0
    , task1
    , task2
    )
where

import           Annotation
import           Deduction
import           Expression
import           Parser
import           System.IO
import           Utility

inFile :: String
inFile = "input.txt"

outFile :: String
outFile = "output.txt"

execTask :: Prs a -> (a -> String) -> IO ()
execTask prs mapper = do
    s <- readFile inFile
    writeFile outFile (mapper $ parse prs (removeSpaces s))

task0 :: IO ()
task0 = execTask expr prefixForm

data AnnotatedLine = AnnLine !Int !AnnotatedExpr

instance Show AnnotatedLine where
    show (AnnLine n ae) = "(" ++ show n ++ ") " ++ show ae

task1 :: IO ()
task1 = do
    s <- readFile inFile
    withFile
        outFile
        WriteMode
        (\handle -> mapM_
            (hPrint handle . uncurry AnnLine)
            (zip [1 ..] (annotateProof (parse proof (removeSpaces s))))
        )

task2 :: IO ()
task2 = execTask proof (show . deduction)

