module Main where

import Control.Monad (void)
import Data.Either (rights)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.ParserCombinators.Parsec (Parser, oneOf, parse, space, (<|>))

data Hand = Rock | Paper | Scissors deriving (Eq)

data Outcome = Win | Loss | Draw deriving (Show)

data Round = Round {player :: Hand, opponent :: Hand}

vs :: Hand -> Hand -> Outcome
Paper `vs` Rock = Win
Rock `vs` Scissors = Win
Scissors `vs` Paper = Win
one `vs` other
  | one == other = Draw
  | otherwise = Loss

score :: Round -> Int
score Round {player, opponent} = handScore player + roundScore (player `vs` opponent)
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

parseHand :: Parser Hand
parseHand =
  Rock <$ oneOf "AX"
    <|> Paper <$ oneOf "BY"
    <|> Scissors <$ oneOf "CZ"

parseRound :: Parser Round
parseRound = do
  opponent <- parseHand
  void $ space
  player <- parseHand
  return Round {player, opponent}

parseInput :: String -> [Round]
parseInput input = rights $ parse parseRound "" <$> (lines input)

main :: IO ()
main = getArgs >>= parseArgs >>= print . sum . map score . parseInput
