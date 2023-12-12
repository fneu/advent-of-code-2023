module Main where

import Data.List
import Data.Char

main :: IO ()
main = do
    -- read stdin into a list of lines
    input <- fmap lines getContents
    -- print first line
    print $ sumOfNumbers input
    -- print $ replaceReversed $ reverse "dvbbjrbfjsxffnjlhfdqthree51oneighttsp"

replaceFromFront :: String -> String
replaceFromFront "" = ""
replaceFromFront ('o':'n':'e':xs) = '1' : replaceFromFront xs
replaceFromFront ('t':'w':'o':xs) = '2' : replaceFromFront xs
replaceFromFront ('t':'h':'r':'e':'e':xs) = '3' : replaceFromFront xs
replaceFromFront ('f':'o':'u':'r':xs) = '4' : replaceFromFront xs
replaceFromFront ('f':'i':'v':'e':xs) = '5' : replaceFromFront xs
replaceFromFront ('s':'i':'x':xs) = '6' : replaceFromFront xs
replaceFromFront ('s':'e':'v':'e':'n':xs) = '7' : replaceFromFront xs
replaceFromFront ('e':'i':'g':'h':'t':xs) = '8' : replaceFromFront xs
replaceFromFront ('n':'i':'n':'e':xs) = '9' : replaceFromFront xs
replaceFromFront (x:xs) = x : replaceFromFront xs

replaceReversed :: String -> String
replaceReversed "" = ""
replaceReversed ('e':'n':'o':xs) = '1' : replaceReversed xs
replaceReversed ('o':'w':'t':xs) = '2' : replaceReversed xs
replaceReversed ('e':'e':'r':'h':'t':xs) = '3' : replaceReversed xs
replaceReversed ('r':'u':'o':'f':xs) = '4' : replaceReversed xs
replaceReversed ('e':'v':'i':'f':xs) = '5' : replaceReversed xs
replaceReversed ('x':'i':'s':xs) = '6' : replaceReversed xs
replaceReversed ('n':'e':'v':'e':'s':xs) = '7' : replaceReversed xs
replaceReversed ('t':'h':'g':'i':'e':xs) = '8' : replaceReversed xs
replaceReversed ('e':'n':'i':'n':xs) = '9' : replaceReversed xs
replaceReversed (x:xs) = x : replaceReversed xs

getFirstDigit :: String -> Char
getFirstDigit = head . filter isDigit

assembleNumber :: Char -> Char -> Int
assembleNumber a b = read [a, b]

sumOfNumbers :: [String] -> Int
sumOfNumbers lines = sum $ map (\line -> assembleNumber (getFirstDigit (replaceFromFront line)) (getFirstDigit (replaceReversed (reverse line)))) lines
