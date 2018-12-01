module Lib
    ( printBoard
    , emptyBoard
    ) where

import Data.List
import Data.Maybe

data Board = Board [Square]

instance Show Board where
  show (Board squares) = show squares

data Piece = Monarch | Hand | Rook | Bishop | Knight | Pawn deriving (Show, Eq)

data Position = Position ValidIndex ValidIndex deriving (Show, Eq)

data Square = Square (Maybe Piece) Position deriving (Eq)

instance Show Square where
  show (Square Nothing _) = " "
  show (Square (Just Monarch) _) = "M"
  show (Square (Just _) _) = "a"

type ValidIndex = Int

data OutOfBoundsError = OutOfBoundsError deriving (Show)

validIndex :: Int -> Either OutOfBoundsError ValidIndex
validIndex n | n < 0 || n > 7 = Left OutOfBoundsError
  | otherwise                 = Right n

emptyBoard :: Board
emptyBoard = Board $ concatMap (\row ->
  map (\column -> Square Nothing $ Position row column) [0..7]
                         ) [0..7]

printBoard :: Board -> IO ()
printBoard board = print board
