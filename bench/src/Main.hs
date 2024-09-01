{-# LANGUAGE OverloadedRecordDot #-}

import Control.Monad.Random.Strict
import Data.Maybe (fromMaybe)
import Field
import Options.Applicative
import Pipes (Producer, (<-<))
import Pipes.Prelude qualified as Pipes
import Player
import System.Random.Shuffle

data Args = Args
  { widthArgs :: !Int,
    heightArgs :: !Int,
    gamesNumberArgs :: !Int,
    seedArgs :: !Int
  }

widthParser :: Parser Int
widthParser = option auto $ long "width" <> short 'w' <> metavar "WIDTH" <> help "Field width"

heightParser :: Parser Int
heightParser = option auto $ long "height" <> short 'h' <> metavar "HEIGHT" <> help "Field width"

gamesNumberParser :: Parser Int
gamesNumberParser = option auto $ long "games-number" <> short 'n' <> metavar "GAMES" <> help "Games number"

seedParser :: Parser Int
seedParser = option auto $ long "seed" <> short 's' <> metavar "SEED" <> help "RNG seed"

argsParser :: Parser Args
argsParser =
  Args
    <$> widthParser
    <*> heightParser
    <*> gamesNumberParser
    <*> seedParser

allMoves :: Int -> Int -> [Pos]
allMoves width' height' = [(x, y) | x <- [0 .. width'], y <- [0 .. height']]

randomGame :: Int -> Int -> Rand StdGen Field
randomGame width' height' = do
  foldr (\pos field -> fromMaybe field $ putNextPoint pos field) (emptyField width' height') <$> shuffleM (allMoves width' height')

randomGames :: Int -> Int -> Int -> Producer Field (Rand StdGen) ()
randomGames games width' height' = Pipes.replicateM games $ randomGame width' height'

data Result = Result
  { redScoreResult :: !Int,
    blackScoreResult :: !Int
  }

instance Semigroup Result where
  (<>) l r = Result (l.redScoreResult + r.redScoreResult) (l.blackScoreResult + r.blackScoreResult)

instance Monoid Result where
  mempty = Result 0 0

gameResult :: Field -> Result
gameResult field = case winner field of
  Just Red -> Result {redScoreResult = 1, blackScoreResult = 0}
  Just Black -> Result {redScoreResult = 0, blackScoreResult = 1}
  Nothing -> Result {redScoreResult = 0, blackScoreResult = 0}

main :: IO ()
main = do
  args <- execParser $ info argsParser (fullDesc <> progDesc "Field benchmark.")
  let gen = mkStdGen args.seedArgs
      result = flip evalRand gen $ Pipes.fold (<>) mempty id $ Pipes.map gameResult <-< randomGames args.gamesNumberArgs args.widthArgs args.heightArgs
  putStrLn $ show result.redScoreResult ++ ":" ++ show result.blackScoreResult
