module Main where

import Control.Monad (void)
import Data.Either (rights)
import Data.Function (on)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Parsec (char, digit, many1, parse)
import Text.Parsec.String (Parser)

data Assignment = Assignment Int Int deriving (Show)

data Assignments = Assignments {elf1 :: Assignment, elf2 :: Assignment} deriving (Show)

overlaps :: Assignments -> Bool
overlaps Assignments {elf1 = Assignment lower1 upper1, elf2 = Assignment lower2 upper2} =
  (lower1 <= upper2 && upper1 >= lower2)
    || (lower2 <= upper1 && upper2 >= lower1)

parseAssignment :: Parser Assignment
parseAssignment =
  do
    lowerBound <- many1 digit
    void $ char '-'
    upperBound <- many1 digit
    pure $ (Assignment `on` read) lowerBound upperBound

parseLine :: Parser Assignments
parseLine =
  do
    elf1 <- parseAssignment
    void $ char ','
    elf2 <- parseAssignment
    pure $ Assignments {elf1, elf2}

parseInput :: String -> [Assignments]
parseInput = rights . (parse parseLine "" <$>) . lines

parseArgs :: [FilePath] -> IO String
parseArgs [input] = readFile input
parseArgs _ = exitFailure

main :: IO ()
main = getArgs >>= parseArgs >>= print . length . filter overlaps . parseInput