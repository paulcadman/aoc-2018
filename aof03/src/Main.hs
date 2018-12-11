{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Data.List ((\\), group, intersect, sort, union)

import qualified Data.Text.Lazy as Text (toStrict)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer as L

readInput :: FilePath -> IO [Text]
readInput path = Text.lines <$> Text.readFile path

type Parser = Parsec Void Text

data Region = Region
  { xMin :: Int
  , xMax :: Int
  , yMin :: Int
  , yMax :: Int
  } deriving (Show)

data Claim = Claim
  { identifier :: Int
  , fromLeft :: Int
  , fromTop :: Int
  , width :: Int
  , height :: Int
  } deriving (Show)

data Segment = Segment
  { sMin :: Int
  , sMax :: Int
  } deriving (Show, Eq, Ord)

data Mode = Start | Stop deriving (Eq, Show)

data BasedSegment = BasedSegment
  { base :: Int
  , segment :: Segment
  , mode :: Mode
  } deriving (Show)

instance Eq BasedSegment where
  s1 == s2 = base s1 == base s2

instance Ord BasedSegment where
  s1 <= s2 = base s1 <= base s2

instance Eq Region where
  r1 == r2 = xMin r1 == xMin r2

instance Ord Region where
  r1 <= r2 = xMin r1 <= xMin r2

parseClaim :: Parser Claim
parseClaim = do
  char '#'
  identifier <- L.decimal
  C.space1
  char '@'
  C.space1
  fromLeft <- L.decimal
  char ','
  fromTop <- L.decimal
  char ':'
  C.space1
  width <- L.decimal
  char 'x'
  height <- L.decimal
  pure $ Claim {..}

mkRegion :: Claim -> Region
mkRegion c =
  let yMin = (fromTop c) + 1
      yMax = (fromTop c) + (height c)
      xMax = (fromLeft c) + (width c)
      xMin = (fromLeft c) + 1
  in Region {..}

mkClaim :: Text -> Claim
mkClaim t =
  case parse parseClaim "" t of
    Left _ -> error "Failed to parse"
    Right claim -> claim

intToText :: Int -> Text
intToText = Text.toStrict . B.toLazyText . B.decimal

putIntLn :: Int -> IO ()
putIntLn = Text.putStrLn . intToText

mkSegment :: Region -> [BasedSegment]
mkSegment r =
  let s =
        let sMin = yMin r
            sMax = yMax r
        in Segment {..} in
    [ BasedSegment {base=(xMin r), segment=s, mode=Start}
    , BasedSegment {base=(xMax r), segment=s, mode=Stop}
    ]

segments :: [Region] -> [BasedSegment]
segments = (mkSegment =<<)

segmentToInterval :: Segment -> [Int]
segmentToInterval s = [sMin s..sMax s]

updateSegment :: [BasedSegment] -> [[Int]] -> [[Int]]
updateSegment bx sx = let
  toAdd = segmentToInterval <$> segment <$> filter (\s -> (mode s == Start)) bx
  toRemove = segmentToInterval <$> segment <$> filter (\s -> (mode s == Stop)) bx in
  (sx \\ toRemove) ++ toAdd

inTwoOrMore :: [[Int]] -> [Int]
inTwoOrMore [] = []
inTwoOrMore (x:xs) = foldr (\a b -> a `union` b) [] [ x `intersect` s | s <- xs ] `union` (inTwoOrMore xs)

calculateSum :: [BasedSegment] -> Int
calculateSum bx = let
  sx = group $ sort bx
  xValues = base <$> bx
  overAllMinX = foldr1 min xValues
  overAllMaxX = foldr1 max xValues
  in
  f [] [overAllMinX..overAllMaxX] sx where
  f _ [] _ = 0
  f _ _ [] = 0
  f activeSegments (n:nx) e@(z:zs) = let zBase = base $ head z in
    if zBase /= n
    then length (inTwoOrMore activeSegments) + f activeSegments nx e
    else
      let
        newSegments = segmentToInterval <$> segment <$> filter (\s -> (mode s == Start)) z
        boundarySegments = activeSegments ++ newSegments
        newActiveSegments = updateSegment z activeSegments
      in
        length (inTwoOrMore boundarySegments) + f newActiveSegments nx zs

solutionOne :: [Text] -> Int
solutionOne =  calculateSum . segments . fmap (mkRegion . mkClaim)

main :: IO ()
main = putIntLn . solutionOne =<< readInput "input.txt"
