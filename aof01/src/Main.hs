module Main where

{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Read (decimal, signed)

import qualified Data.Text.Lazy as Text (toStrict)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B

import Data.Set (Set)
import qualified Data.Set as Set

readInput :: FilePath -> IO [Text]
readInput path = Text.lines <$> Text.readFile path

parseInt :: Text -> Int
parseInt entry = case parsed of
  Left s -> error $ "could not parse text" <> s
  Right number -> number
  where
    parsed = fst <$> (signed $ decimal) entry

first :: Either Text (Int, Int) -> Either Text Int
first = fmap fst

findDuplicate :: [Int] -> Int
findDuplicate l = f Set.empty l where
  f :: Set Int -> [Int] -> Int
  f s [] = error "Should find dup"
  f s (x:xs) = if (Set.member x s) then x else f (Set.insert x s) xs

applySequence :: [Int] -> [Int]
applySequence = scanl (+) 0

intToText :: Int -> Text
intToText = Text.toStrict . B.toLazyText . B.decimal

putIntLn :: Int -> IO ()
putIntLn = Text.putStrLn . intToText

inputToInts :: IO [Int]
inputToInts = (fmap . fmap) parseInt (readInput "input")

solutionOne :: [Int] -> Int
solutionOne = sum

solutionTwo :: [Int] -> Int
solutionTwo = findDuplicate . applySequence . cycle

main :: IO ()
main = do
  ints <- inputToInts
  putIntLn . solutionOne $ ints
  putIntLn . solutionTwo $ ints
