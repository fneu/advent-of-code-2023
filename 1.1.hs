module Main where

import Data.List
import Data.Char

main :: IO ()
main = do
    -- read stdin into a list of lines
    input <- fmap lines getContents
    -- print first line
    print $ sumOfNumbers input


getFirstDigit :: String -> Char
getFirstDigit = head . filter isDigit

getLastDigit :: String -> Char
getLastDigit = last . filter isDigit

assembleNumber :: Char -> Char -> Int
assembleNumber a b = read [a, b]

sumOfNumbers :: [String] -> Int
sumOfNumbers lines = sum $ map (\line -> assembleNumber (getFirstDigit line) (getLastDigit line)) lines
