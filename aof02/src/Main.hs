module Main where

{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow ((***))
import Data.Text (Text)
import Data.List (nub)
import Data.String (IsString, fromString)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Read (decimal, signed)

import qualified Data.Text.Lazy as Text (toStrict)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

readInput :: FilePath -> IO [Text]
readInput path = Text.lines <$> Text.readFile path

countOccurances :: Text -> Map Char Int
countOccurances t = Map.fromListWith (+) [(c, 1) | c <- (Text.unpack t)]

numberOfRepeats :: Int -> Map Char Int -> Int
numberOfRepeats n m = length $ Map.keys (Map.filter (== n) m)

intToText :: Int -> Text
intToText = Text.toStrict . B.toLazyText . B.decimal

putIntLn :: Int -> IO ()
putIntLn = Text.putStrLn . intToText

solutionOne :: [Text] -> Int
solutionOne tx = (fst r) * (snd r) where
  r = foldr c (0,0) tx
  c :: Text -> (Int,Int) -> (Int,Int)
  c t (twice, thrice) = case (numberOfRepeats 2 occur, numberOfRepeats 3 occur) of
    (0, 0) -> (twice, thrice)
    (0, _) -> (twice, thrice+1)
    (_, 0) -> (twice+1, thrice)
    otherwise -> (twice+1, thrice+1)
    where
      occur = countOccurances t

newtype Identifier = Identifier Text deriving Show

unIdentifier :: Identifier -> Text
unIdentifier (Identifier t) = t

instance IsString Identifier where
  fromString = Identifier . Text.pack

instance Eq Identifier where
  Identifier a == Identifier b = let
    zipped = zip (Text.unpack a) (Text.unpack b) in
    snd $ foldr c (False, False) zipped
    where
    c (c1, c2) orig@(hasDiffered, isEqual)
      | c1 == c2 = orig
      | c1 /= c2 = if hasDiffered then (hasDiffered, False) else (True, True)

duplicates :: Eq a => [a] -> [(a,a)]
duplicates [] = []
duplicates (x:xs) = case [(x,y) | y <- xs, y == x] of
  (b:ys) -> [b]
  [] -> duplicates xs

common :: (Text, Text) -> Text
common (t1, t2) = Text.pack $ f (zip (Text.unpack t1) (Text.unpack t2)) where
  f [] = []
  f ((x,y):zs)
    | x == y = x : f zs
    | otherwise = f zs

solutionTwo :: [Text] -> Text
--solutionTwo = unIdentifier . firstDuplicate . (fmap Identifier)
solutionTwo = common . head . (fmap (unIdentifier *** unIdentifier)) . (duplicates . (fmap Identifier))

main :: IO ()
main = do
  tx <- (readInput "input.txt")
  putIntLn . solutionOne $ tx
  Text.putStrLn . solutionTwo $ tx
