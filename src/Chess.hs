module Chess
    ( printBoard
    , emptyBoard
    , place
    , whiteKnight
    , remove
    , a1
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

whiteKnight :: Piece
whiteKnight =
  Piece White Knight

a1 :: Position
a1 = Position 0 0

place :: Piece -> Position -> Board -> Board
place piece position (Board board) =
  Board $ map (\square -> addPieceToSquare piece position square) board

remove :: Position -> Board -> Board
remove position (Board board) =
  Board $ map (\square -> clearSquare position square) board

emptyBoard :: Board
emptyBoard = Board $ concatMap (\row ->
  map (\column -> Square Nothing $ Position row column) [0..7]
                         ) [0..7]

printBoard :: Board -> IO ()
printBoard board =
  print $ byRow board

-- PRIVATE --

byRow :: Board -> [[Square]]
byRow (Board board) =
  [(take 8 board), (take 8 board)]

mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex fn list = zipWith fn list [0..]

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
