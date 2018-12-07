module Code3 where

import Data.List
import qualified Data.List as List
import Data.List.Split
import Data.Matrix
import Data.Vector
import qualified Data.Vector as Vector

data Claim = Claim Id At Rect
  deriving (Show)

data Id = Id String
  deriving (Show)

type At = (Int, Int)
type Rect = (Int, Int)
type Fabric = Matrix String

--- EXAMPLE DATA ---
ex = "#1 @ 236,827: 24x17"

ex2 = ["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"]
ex3 = ["#1 @ 1,3: 4x4", "#3 @ 5,5: 2x2"]

ex4 = ["#1 @ 1,3: 4x4", "#2 @ 3,1: 5x5", "#3 @ 5,5: 2x2"]

exClaim1 = Claim (Id "1") (1, 3) (4,4)
exClaim3 = Claim (Id "3") (5, 5) (2,2)

sizeEx = 8
------


main :: IO ()
main = do
  contents <- readFile "input_3.txt"
  let c = lines contents
  let claims = toClaims c
  let markedFab = markClaims claims
  putStrLn $ show (countSquareInches markedFab)


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


--- MARKING CLAIMS ON FABRIC (MATRIX STRING) ---

initFabric :: Int ->  Fabric
initFabric size = fromLists (List.replicate 1000 (List.replicate 1000 "."))

markFabric :: Claim -> Fabric -> Fabric
markFabric (Claim (Id id) (c,r) (j,i)) fab = markFabric' id pos fab  -- use markFabric'
    where pos = [ (r + i', c + j') | j' <- [0..(j-1)], i' <- [0..(i-1)]]

getPos :: Claim -> [(Int, Int)]
getPos (Claim _ (r,c) (x,y)) = [ (r + x' , c + y') | x' <- [0..(x-1)], y' <- [0..(y-1)]]

markFabric' :: String -> [(Int, Int)] -> Fabric -> Fabric
markFabric' id [] fab = fab
markFabric' id ((y,x):xs) fab = markFabric' id xs fab'
  where fab' = case (getElem y x fab) of
          "." -> setElem id (y,x) fab
          _   -> setElem "X" (y,x) fab

markClaims :: [Claim] -> Fabric
markClaims xs = List.foldr markFabric (initFabric sizeEx) xs


--- FINDING "X" LOCATIONS ---

findXCol :: Int -> Fabric -> Int
findXCol n fab
  | n == (nrows fab) = -1
  | otherwise = case (Vector.findIndex (=="X") (getRow n fab)) of
                  Just x -> x
                  Nothing -> findXCol (n+1) fab

findXRow :: Int -> Fabric -> Int
findXRow n fab
  | n == nrows fab = -1
  | otherwise = case (Vector.findIndex (=="X") (getRow n fab)) of
      Just x -> x
      Nothing -> findXRow (n+1) fab

findXStart :: Fabric -> Maybe (Int, Int)
findXStart fab = case (findXRow 1 fab) of
  -1 -> Nothing
  r  -> Just (r, findXCol 1 fab)


--- COUNTING X's ---

countX :: Int -> Fabric -> Int
countX r fab = count
  where v = getRow r fab
        count = List.length $ List.filter (=="X") (Vector.toList v)

countY :: Int -> Fabric -> Int
countY c fab = count
  where v = getCol c fab
        count = List.length $ List.filter (=="X") (Vector.toList v)

countSquareInches :: Fabric -> Int
countSquareInches fab = case findXStart fab of
  Nothing    -> 0
  Just (r,c) -> (countX (r+1) fab) * (countY (c+1) fab) 





