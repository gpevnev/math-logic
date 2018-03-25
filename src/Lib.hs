module Lib
    ( task0
    , task1
    ) where

import Expression
import Parser

removeSpaces :: String -> String
removeSpaces = foldr f "" where 
    f c acc | (c == ' ' || c == '\t' || c == '\r') = acc
            | otherwise = c : acc

inFile :: String
inFile = "input.txt"

outFile :: String
outFile = "output.txt"

execTask :: Show b => Prs a -> (a -> b) -> IO ()
execTask prs mapper = do 
    s <- readFile inFile
    writeFile outFile (show $ mapper $ parse prs (removeSpaces s))

task0 :: IO ()
task0 = execTask expr id

task1 :: IO ()
task1 = execTask file id
