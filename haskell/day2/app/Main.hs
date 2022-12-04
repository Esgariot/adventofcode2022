{-# LANGUAGE DataKinds #-}

module Main where

import Control.Monad (void)
import Data.Either (rights)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.ParserCombinators.Parsec (Parser, char, parse, space, (<|>))

data Player = Me | Opponent

data Hand a = Rock | Paper | Scissors deriving (Eq)

data Outcome = Win | Loss | Draw deriving (Eq)

data Round = Round {opponent :: Hand Opponent, outcome :: Outcome}

vs :: Hand Me -> Hand Opponent -> Outcome
Paper `vs` Rock = Win
Rock `vs` Scissors = Win
Scissors `vs` Paper = Win
one `vs` other
  | un one == un other = Draw
  | otherwise = Loss
  where
    un Rock = Rock
    un Paper = Paper
    un Scissors = Scissors

strategy :: Round -> Hand Me
strategy Round {opponent, outcome} = head $ (filter $ match outcome) [Rock, Paper, Scissors]
  where
    match expected hand = expected == hand `vs` opponent

score :: Hand Me -> Hand Opponent -> Int
score player opponent = handScore player + roundScore (player `vs` opponent)
  where
    handScore Rock = 1
    handScore Paper = 2
    handScore Scissors = 3
    roundScore Loss = 0
    roundScore Draw = 3
    roundScore Win = 6

parseArgs :: [FilePath] -> IO String
parseArgs [input] = readFile input
parseArgs _ = exitFailure

parseHand :: Parser (Hand a)
parseHand =
  Rock <$ char 'A'
    <|> Paper <$ char 'B'
    <|> Scissors <$ char 'C'

parseOutcome :: Parser Outcome
parseOutcome =
  Loss <$ char 'X'
    <|> Draw <$ char 'Y'
    <|> Win <$ char 'Z'

parseRound :: Parser Round
parseRound = do
  opponent <- parseHand
  void $ space
  outcome <- parseOutcome
  return Round {opponent, outcome}

parseInput :: String -> [Round]
parseInput input = rights $ parse parseRound "" <$> (lines input)

main :: IO ()
main = getArgs >>= parseArgs >>= print . sum . map (\r@Round {opponent} -> score (strategy r) opponent) . parseInput
