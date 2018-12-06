module Code3 where

import Data.List
import Data.List.Split
import Data.Matrix

ex = "#1 @ 236,827: 24x17"

ex2 = ["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"]

data Claim = Claim Id At Rect
  deriving (Show)

data Id = Id String
  deriving (Show)

type At = (Int, Int)
type Rect = (Int, Int)
type Fabric = Matrix String

sizeEx = 8

main :: IO ()
main = do
  contents <- readFile "input3test.txt"
  let c = lines contents
  putStrLn $ show c
  let g = map group c
  putStrLn $ show g


toClaim :: String -> Claim
toClaim str = Claim (Id id) at re
  where xs = words str
        id = getID (xs !! 0)
        at = getAts (xs !! 2)
        re = getRects (xs !! 3)

toClaims :: [String] -> [Claim]
toClaims xs = map toClaim xs

getID :: String -> String
getID ('#':rest) = rest

getAts :: String -> (Int, Int)
getAts str = (read x, read y)
  where clean  = take (length str - 1) str
        (x, y) = (head (splitOn "," clean), last (splitOn "," clean)) 
  
getRects :: String -> (Int, Int)
getRects str = (read x, read y)
  where (x, y) = (head (splitOn "x" str), last (splitOn "x" str))

initFabric :: Int ->  Fabric
initFabric size = fromLists (replicate size (replicate size "."))

markFabric :: Claim -> Fabric -> Fabric
markFabric (Claim (Id id) (x,y) (i,j)) fab = undefined -- use markFabric'
    where pos = [ (x + i', y + j') | i' <- [0..i], j' <- [0..j]]

markFabric' :: String -> [(Int, Int)] -> Fabric
markFabric' str [] fab = fab
markFabric' str (x:xs) fab = undefined

markClaims :: [Claim] -> Fabric
markClaims xs = undefined


