module Chess
    ( printBoard
    , emptyBoard
    , playBoard
    , place
    , whiteKnight
    , remove
    , move
    , get
    , a1, a2
    , h8
    ) where

import Data.List
import Data.Maybe

data Board = Board [Square] deriving (Eq)

instance Show Board where
  show (Board squares) = show squares

data PieceType = Monarch | Hand | Rook | Bishop | Knight | Pawn deriving (Show, Eq)

data Piece = Piece Team PieceType deriving (Eq, Show)

data Team = White | Black deriving (Eq, Show)

data Position = Position Int Int deriving (Show, Eq)

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

whiteKnight :: Piece
whiteKnight =
  Piece White Knight

whiteBishop :: Piece
whiteBishop =
  Piece White Bishop

whiteHand :: Piece
whiteHand =
  Piece White Hand

a1 :: Position
a1 = Position 0 0

a2 :: Position
a2 = Position 1 0

h8 :: Position
h8 = Position 7 7

place :: Piece -> Position -> Board -> Board
place piece position (Board board) =
  Board $ map (\square -> addPieceToSquare piece position square) board

remove :: Position -> Board -> Board
remove position (Board board) =
  Board $ map (\square -> clearSquare position square) board

move :: Position -> Position -> Board -> Board
move from to board =
  case get from board of
    Nothing ->
      board
    Just piece ->
      (place piece to . remove from) board

get :: Position -> Board -> Maybe Piece
get position (Board board) =
  let
    (Square maybePiece _) =
      head $ filter (\square -> squareMatchesPosition position square) board
  in
  maybePiece

emptyBoard :: Board
emptyBoard = Board $ concatMap (\row ->
  map (\column -> Square Nothing $ Position row column) [0..7]
                         ) [0..7]

printBoard :: Board -> IO ()
printBoard board =
  print $ rows board

playBoard :: Board
playBoard = foldr (\(pos, piece) currentBoard -> place piece pos currentBoard)
                emptyBoard
                [(a1, whiteKnight), (a2, whiteBishop), (h8, whiteHand)]

-- PRIVATE --

rows :: Board -> [[Square]]
rows (Board board) =
  reverse $ map (row board) [0..7]

row :: [Square] -> Int -> [Square]
row rawBoard index =
  (take 8 . drop (8 * index)) rawBoard

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

squareMatchesPosition :: Position -> Square -> Bool
squareMatchesPosition position (Square maybePiece currentPosition) =
  position == currentPosition
