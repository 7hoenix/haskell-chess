module Lib
    ( printBoard
    , emptyBoard
    ) where

data Board = Board Position

instance Show Board where
  show (Board pos) = show pos

data Piece = Monarch | Hand | Rook | Bishop | Knight | Pawn deriving (Show, Eq)

data Position = Position ValidIndex ValidIndex deriving (Show, Eq)

type ValidIndex = Int

data OutOfBoundsError = OutOfBoundsError deriving (Show)

validIndex :: Int -> Either OutOfBoundsError ValidIndex
validIndex n | n < 0 || n > 7 = Left OutOfBoundsError
  | otherwise                 = Right n

emptyBoard :: Board
emptyBoard = Board $ Position 0 0

printBoard :: Board -> IO ()
printBoard board = putStrLn $ show board
