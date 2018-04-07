module Lib
    ( task0
    , task1
    ) where

import Expression
import Annotation
import System.IO
import Parser

removeSpaces :: String -> String
removeSpaces = foldr f "" where 
    f c acc | (c == ' ' || c == '\t' || c == '\r') = acc
            | otherwise = c : acc

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

task1 :: IO ()
task1 = do 
    s <- readFile inFile
    withFile outFile WriteMode (\handle -> 
        mapM_ (hPutStrLn handle) $ annotateProof (parse file (removeSpaces s))
        )
