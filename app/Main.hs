module Main where

import Chess
import Text.Trifecta.Parser



-- parseSquare :: Parser

main :: IO ()
main = do
  printBoard playBoard
  putStrLn "Enter square"
  square <- getLine
  -- putStrLn "Enter row"
  -- row <- getLine

  let
    parsedSquare = parseString parsePosition mempty square
  print parsedSquare
  printBoard playBoard
