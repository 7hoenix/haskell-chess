module Chess
    ( printBoard
    , emptyBoard
    , place
    , whiteKnight
    , validPosition
    , remove
    ) where

import Data.List
import Data.Maybe

data Board = Board [Square] deriving (Eq)

instance Show Board where
  show (Board squares) = show squares

data PieceType = Monarch | Hand | Rook | Bishop | Knight | Pawn deriving (Show, Eq)

data Piece = Piece Team PieceType deriving (Eq, Show)

data Team = White | Black deriving (Eq, Show)

data Position = Position ValidIndex ValidIndex deriving (Show, Eq)

data Square = Square (Maybe Piece) Position deriving (Eq)

instance Show Square where
  show (Square Nothing _) = " "
  show (Square (Just (Piece White Monarch)) _) = "M"
  show (Square (Just (Piece White Hand)) _) = "H"
  show (Square (Just (Piece White Rook)) _) = "R"
  show (Square (Just (Piece White Bishop)) _) = "B"
  show (Square (Just (Piece White Knight)) _) = "N"
  show (Square (Just (Piece White Pawn)) _) = "P"
  show (Square (Just _) _) = "z"

type ValidIndex = Int

data OutOfBoundsError = OutOfBoundsError deriving (Show)

whiteKnight :: Piece
whiteKnight =
  Piece White Knight

validIndex :: Int -> Either OutOfBoundsError ValidIndex
validIndex n | n < 0 || n > 7 = Left OutOfBoundsError
  | otherwise                 = Right n

validPosition :: Int -> Int -> Either OutOfBoundsError Position
validPosition row column =
  case (validIndex row, validIndex column) of
    (Left _, _) -> Left OutOfBoundsError
    (_, Left _) -> Left OutOfBoundsError
    (Right validRow, Right validColumn) -> Right $ Position validRow validColumn

emptyBoard :: Board
emptyBoard = Board $ concatMap (\row ->
  map (\column -> Square Nothing $ Position row column) [0..7]
                         ) [0..7]

printBoard :: Board -> IO ()
printBoard board = print board

place :: Piece -> Position -> Board -> Board
place piece position (Board board) =
  Board $ map (\square -> addPieceToSquare piece position square) board

remove :: Position -> Board -> Board
remove position (Board board) =
  Board $ map (\square -> clearSquare position square) board

addPieceToSquare :: Piece -> Position -> Square -> Square
addPieceToSquare piece position (Square maybePiece currentPosition)
  | matches = (Square (Just piece) currentPosition)
  | otherwise = (Square maybePiece currentPosition)
  where matches = position == currentPosition

clearSquare :: Position -> Square -> Square
clearSquare position (Square maybePiece currentPosition)
  | matches = (Square Nothing currentPosition)
  | otherwise = (Square maybePiece currentPosition)
  where matches = position == currentPosition
