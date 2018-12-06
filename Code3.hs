module Code3 where

import Data.List
import Data.Char
import Data.Maybe
import System.IO

ex = "#1 @ 236,827: 24x17"

data Claim = Claim ID AT RECT
  deriving (Show)

data ID = ID Int
  deriving (Show)

type AT = (Int, Int)
type RECT = (Int, Int)

main :: IO ()
main = do
  contents <- readFile "input3test.txt"
  let c = lines contents
  putStrLn $ show c
  let g = map group c
  putStrLn $ show g


toClaim :: String -> Maybe Claim
toClaim ('#' : id : '@' : x : y : ':' : n1 : 'x' : n2) = undefined
