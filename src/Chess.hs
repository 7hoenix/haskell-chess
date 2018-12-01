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

whiteKnight :: Piece
whiteKnight =
  Piece White Knight

validPosition :: Int -> Int -> Either String Position
validPosition row column =
  case (validIndex row, validIndex column) of
    (Left _, _) -> Left $ "Row out of bounds: " ++ show row
    (_, Left _) -> Left $ "Column out of bounds: " ++ show column
    (Right validRow, Right validColumn) -> Right $ Position validRow validColumn

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
  -- map (\row -> putStrLn row) $ byRow board

-- PRIVATE --

byRow :: Board -> [[Square]]
byRow (Board board) =
  [(take 8 board)]
  -- mapWithIndex (\square index -> asdf square index) board

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

validIndex :: Int -> Either String ValidIndex
validIndex n | n < 0 || n > 7 = Left $ "Index out of bounds: " ++ show n
  | otherwise                 = Right n
