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

-- parseExprFromHandle :: Handle -> IO Expr
-- parseExprFromHandle handle = do 
--     l <- hGetLine handle
--     return $ parse expr (removeSpaces l)

-- parseExprs :: Handle -> IO [Expr]
-- parseExprs handle = do 
--     eof <- hIsEOF handle
--     if not eof then (:) <$> (parseExprFromHandle handle) <*> parseExprs handle else return []

task1 :: IO ()
task1 = do 
    s <- readFile inFile
    withFile outFile WriteMode (\handle -> annotateProof (parse file (removeSpaces s)) handle)
-- task1 = do 
--     -- let fileObj = parse 
--     fileObj <- withFile inFile ReadMode (\handle -> do
--         headerLine <- hGetLine handle
--         let header' = (parse header (removeSpaces headerLine))
--         File header' <$> (parseExprs handle)
--         )
--     withFile outFile WriteMode (\handle -> annotateProof fileObj handle)
