module Main where

import Data.Char
import Data.List

trim = dropWhileEnd isSpace . dropWhile isSpace

elim :: String -> String
elim [] = []
elim arg@[_] = arg
elim (x:y:xs)
  | sameChar && isUpper x && isLower y = elim xs
  | sameChar && isLower x && isUpper y = elim xs
  | otherwise = x : elim (y : xs)
  where
    sameChar = toUpper x == toUpper y

elimAll :: String -> String
elimAll s =
  let next = elim s
  in if s == next
       then s
       else elimAll next

solutionOne :: String -> Int
solutionOne = length . elimAll

removeAll :: Char -> String -> String
removeAll c = filter (\x -> (x /= toUpper c) && (x /= toLower c))

solutionTwo :: String -> Int
solutionTwo all' = aux [] ['a' .. 'z']
  where
    aux results [] = minimum results
    aux results (x:xs) =
      let xRemoved = removeAll x all'
          xElim = length $ elimAll xRemoved
      in aux (xElim : results) xs

main :: IO ()
main = do
  input <- readFile "input.txt"
  let printSol solution = print $ solution $ trim input
  printSol solutionOne
  printSol solutionTwo
