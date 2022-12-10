{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Function (on)
import System.Environment (getArgs)
import Data.Set qualified as Set (elems, fromList, intersection)
import System.Exit (exitFailure)

data Rucksack a = Rucksack {compartment1 :: a, compartment2 :: a} deriving (Show)

makeRucksack :: [a] -> Rucksack [a]
makeRucksack xs = Rucksack {compartment1 = take half xs, compartment2 = take half (reverse xs)}
  where
    half = length xs `div` 2

findError :: Ord a => Rucksack [a] -> a
findError Rucksack {compartment1, compartment2} =
  (head . Set.elems) $ intersection compartment1 compartment2
  where
    intersection = Set.intersection `on` Set.fromList

priority :: Char -> Int
priority n
  | 'a' <= n && n <= 'z' = 'a' `minus` n + 1
  | 'A' <= n && n <= 'Z' = 'A' `minus` n + 27
  | otherwise = error "Unexpected letter"
  where
    minus = flip ((-) `on` fromEnum)

parseRucksacks :: String -> [Rucksack String]
parseRucksacks = (makeRucksack <$>) . lines

parseArgs :: [FilePath] -> IO String
parseArgs [input] = readFile input
parseArgs _ = exitFailure

main :: IO ()
main = getArgs >>= parseArgs >>= print . sum . (priority . findError <$>) . parseRucksacks
