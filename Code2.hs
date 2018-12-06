module Code2 where

import Data.Char
import Data.List
import System.IO


doIt :: IO ()
doIt = do 
  contents <- readFile "input_2.txt"
  let c = lines contents
  let dubs = getAllDubs c
  let trips = getAllTrips c
  putStrLn $ "Solution part 1: " ++ show (length dubs * length trips)
  let p2 = getCFA c
  putStrLn $ show p2

getDubs :: String -> [String]
getDubs str = filter (\x -> length x == 2) (group (sort str))

getTrips :: String -> [String]
getTrips str = filter (\x -> length x == 3) (group (sort str))

getAllDubs :: [String] -> [[String]]
getAllDubs = filter (/= []) .map getDubs

getAllTrips :: [String] -> [[String]]
getAllTrips = filter (/= []) . map getTrips

-------------------------------------------
ex = ["abcdef","bababc","abbcde","abcccd","aabcdd","abcdee","ababab"]
ex2 = ["aa","aaa", "aa"]
ex3 = ["abcde", "fghij", "klmno", "pqrst","fguij","axcye","wvxyz"]
-----------------------------------------------
-- NOT MY CODE -- 
part1 :: String -> Int
part1 input = length (filter (any (==2)) letterCounts)
              * length (filter (any (==3)) letterCounts)
    where letterCounts = map (map length . group . sort) $ lines input

--- PART 2 ---

-- differ correctly
differC :: String -> String -> Bool
differC s1 s2 = length s1 - length comp == 1
  where comp = filter (`elem` (sort s2)) (sort s1)

differ :: String -> String -> Int
differ s1 s2 = length s1 - length comp
  where comp = filter (`elem` s2) s1

getCommon :: String -> String -> String
getCommon [] [] = []
getCommon (x:xs) (c:cs)
  | x == c = x : getCommon xs cs
  | otherwise = getCommon xs cs


getCFA :: [String] -> [String]
getCFA xs = fil
  where done = nub $ [getCommon s1 s2 | s1 <- xs, s2 <- xs, differC s1 s2]
        fil  = filter (\x -> length x == 25) done
