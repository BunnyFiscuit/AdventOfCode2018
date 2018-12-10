module Code32 where

import Data.List
import qualified Data.List as List
import Data.List.Split
import Data.Map
import qualified Data.Map as Map


data Claim = Claim Id At Rect
  deriving (Show)

data Id = Id String
  deriving (Show)

type At = (Int, Int)
type Rect = (Int, Int)
type Fabric = Map (Int, Int) String

--- EXAMPLE DATA ---
ex = "#1 @ 236,827: 24x17"

ex2 = ["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"]
ex22 = ["#1 @ 10,30: 40x40", "#2 @ 30,10: 40x40", "#3 @ 50,50: 20x20"]
ex3 = ["#1 @ 1,3: 4x4", "#3 @ 5,5: 2x2"]

ex4 = ["#1 @ 1,3: 4x4", "#2 @ 3,1: 5x5", "#3 @ 5,5: 2x2"]

exClaim1 = Claim (Id "1") (1, 3) (4,4)
exClaim3 = Claim (Id "3") (5, 5) (2,2)

sizeEx = 80
------


main :: IO ()
main = do
  contents <- readFile "input_3.txt"
  let c = lines contents
  let claims = toClaims c
  putStrLn $ show (List.length claims)
  let res = Map.filter (=="X") (markClaims claims initFabric)
  putStrLn $ show (Map.size res)
  return ()


--- STRING TO CLAIM CONVERSION ---
toClaim :: String -> Claim
toClaim str = Claim (Id id) at re
  where xs = words str
        id = getID (xs !! 0)
        at = getAts (xs !! 2)
        re = getRects (xs !! 3)

toClaims :: [String] -> [Claim]
toClaims xs = List.map toClaim xs

getID :: String -> String
getID ('#':rest) = rest

getAts :: String -> (Int, Int)
getAts str = (read x + 1, read y + 1)
  where clean  = List.take (List.length str - 1) str
        (x, y) = (List.head (splitOn "," clean), List.last (splitOn "," clean))

getRects :: String -> (Int, Int)
getRects str = (read x, read y)
  where (x, y) = (List.head (splitOn "x" str), List.last (splitOn "x" str))

-- INSERT CLAIMS --

initFabric :: Fabric
initFabric = empty

markFabric :: Claim -> Fabric -> Fabric
markFabric (Claim (Id id) (c,r) (w,h)) fab = markFabric' id pos fab
  where pos =[ (r + h' , c + w') | w' <- [0..(w-1)], h' <- [0..(h-1)]]

markFabric' :: String -> [(Int, Int)] -> Fabric -> Fabric
markFabric' id [] fab = fab
markFabric' id ( p@(c,r) : xs ) fab = markFabric' id xs fab'
  where fab' = case (Map.lookup p fab) of
          Just _ -> (Map.insert p "X" fab)
          Nothing -> (Map.insert p id fab)

markClaims :: [Claim] -> Fabric -> Fabric
markClaims [] fab = fab
markClaims (c:cs) fab = markClaims cs fab'
  where fab' = markFabric c fab 


