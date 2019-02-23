module Main where

import Chess
import Text.Trifecta.Parser



-- parseSquare :: Parser

main :: IO ()
main = do
  printBoard playBoard
  putStrLn "Enter col"
  col <- getLine
  putStrLn "Enter row"
  row <- getLine

  parsedCol <- parseString parsePosition mempty "asdf"
  putStrLn $ unwords [col, row]
  printBoard playBoard
