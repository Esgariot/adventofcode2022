{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Function (on)
import Data.Set qualified as Set (elems, fromList, intersection)
import System.Environment (getArgs)
import System.Exit (exitFailure)

data Group a = Group a a a deriving (Show)

findBadge :: Ord a => Group [a] -> a
findBadge (Group elf1 elf2 elf3) =
  (head) $ elf1 /\ elf2 /\ elf3
  where
    (/\) = (Set.elems <$>) . Set.intersection `on` Set.fromList

priority :: Char -> Int
priority n
  | 'a' <= n && n <= 'z' = 'a' `minus` n + 1
  | 'A' <= n && n <= 'Z' = 'A' `minus` n + 27
  | otherwise = error "Unexpected letter"
  where
    minus = flip ((-) `on` fromEnum)

parseGroups :: String -> [Group String]
parseGroups = groupUp . lines
  where
    groupUp [] = []
    groupUp (a : b : c : xs) = Group a b c : groupUp xs
    groupUp _ = error "Unexpected group count"

parseArgs :: [FilePath] -> IO String
parseArgs [input] = readFile input
parseArgs _ = exitFailure

main :: IO ()
main = getArgs >>= parseArgs >>= print . sum . (priority . findBadge <$>) . parseGroups
