{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (isSpace)
import Data.List (sortOn)
import Data.List.Split (splitWhen)
import Data.Ord (Down (Down))
import System.Environment (getArgs)
import System.Exit (exitFailure)

parseArg :: [FilePath] -> IO String
parseArg [input] = readFile input
parseArg _ = exitFailure

parseGroups :: String -> [[Int]]
parseGroups xs = parse <$> (groupInputs xs)
  where
    groupInputs = splitWhen (all isSpace) . lines
    parse = map (read :: String -> Int)

top :: Ord a => Int -> [a] -> [a]
top n = take n . sortOn Down

main :: IO ()
main = getArgs >>= parseArg >>= print . sum . top 3 . (map sum <$> parseGroups)
