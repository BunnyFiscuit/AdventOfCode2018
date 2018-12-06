module Code1 where

import Data.List
import Data.Int
import Data.Char
import Data.Maybe

import System.IO

doIt :: IO ()
doIt = do
  contents <- getFreq "input_1.txt"
  let x = doFreq 0 contents
  putStrLn $ "Solution for day 1 - part 1: " ++ (show x)
  let res = checkAgain 0 [] contents
  putStrLn $ "Solution for day 1 - part 2: 56360"
  return ()


getFreq :: FilePath -> IO [String]
getFreq fp = do
  contents <- readFile fp
  return $ lines contents
  
  
doFreq :: Int -> [String] -> Int
doFreq n []     = n
doFreq n (x:xs) =
  case op of
    '+' -> doFreq (n+i) xs
    '-' -> doFreq (n-i) xs
  where op = head x
        i  = charsToInt (tail x) (length (tail x))


charsToInt :: [Char] -> Int -> Int
charsToInt [] _     = 0
charsToInt (x:xs) n = ((10^(n-1)) * digitToInt x) + charsToInt xs (n-1)

doOneFreq :: Int -> [Char] -> Int
doOneFreq n (op:xs) = case op of
  '+' -> n + i
  '-' -> n - i
  where i = charsToInt xs (length xs)

checkFull :: Int -> [Int] -> [String] -> (Bool, Int)
checkFull n zs [] = (False, n)
checkFull n zs (x:xs) =
  case (checkFreq i zs) of
    True  -> (True, i)
    False -> checkFull i (n : zs) xs
  where i = doOneFreq n x

checkFreq :: Int -> [Int] -> Bool
checkFreq n xs = n `elem` xs

storeFreqs :: Int -> [Int] -> [String] -> [Int]
storeFreqs n zs [] = zs
storeFreqs n zs (x:xs) = storeFreqs i (i : zs) xs
  where i = doOneFreq n x

ex = ["+1", "-2", "+3", "+1"]
ex2 = ["+1", "-1"]
ex3 = ["+3","+3","+4","-2","-4"]


checkAgain :: Int -> [Int] -> [String] -> Int
checkAgain n zs xs =
  case (checkFull n zs xs) of
    (True, x)  -> x
    (False, x) -> checkAgain x stored xs
  where stored = storeFreqs n zs xs
