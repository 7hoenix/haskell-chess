module Chess
    ( printBoard
    , emptyBoard
    , get
    , move
    , place
    , playBoard
    , remove
    , legal
    , Team(..)
    , monarch, hand, rook, bishop, knight, pawn
    , a1, a2, a3, a4, a5, a6, a7, a8
    , b1, b2, b3, b4, b5, b6, b7, b8
    , c1, c2, c3, c4, c5, c6, c7, c8
    , d1, d2, d3, d4, d5, d6, d7, d8
    , e1, e2, e3, e4, e5, e6, e7, e8
    , f1, f2, f3, f4, f5, f6, f7, f8
    , g1, g2, g3, g4, g5, g6, g7, g8
    , h1, h2, h3, h4, h5, h6, h7, h8
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

data Direction = N | NE | E | SE | S | SW | W | NW deriving (Eq)

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

legal :: Position -> Board -> [Position]
legal position board =
  case get position board of
    Nothing -> []
    Just (Piece _ Monarch) -> legalMonarch position board
    Just (Piece _ Rook) -> legalRook position board
    _ -> []

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

-- Piece constructor functions

monarch :: Team -> Piece
monarch team = Piece team Monarch

hand :: Team -> Piece
hand team = Piece team Hand

rook :: Team -> Piece
rook team = Piece team Rook

bishop :: Team -> Piece
bishop team = Piece team Bishop

knight :: Team -> Piece
knight team = Piece team Knight

pawn :: Team -> Piece
pawn team = Piece team Pawn

-- All Possible positions

a1 :: Position
a1 = Position 0 0

a2 :: Position
a2 = Position 1 0

a3 :: Position
a3 = Position 2 0

a4 :: Position
a4 = Position 3 0

a5 :: Position
a5 = Position 4 0

a6 :: Position
a6 = Position 5 0

a7 :: Position
a7 = Position 6 0

a8 :: Position
a8 = Position 7 0


b1 :: Position
b1 = Position 0 1

b2 :: Position
b2 = Position 1 1

b3 :: Position
b3 = Position 2 1

b4 :: Position
b4 = Position 3 1

b5 :: Position
b5 = Position 4 1

b6 :: Position
b6 = Position 5 1

b7 :: Position
b7 = Position 6 1

b8 :: Position
b8 = Position 7 1


c1 :: Position
c1 = Position 0 2

c2 :: Position
c2 = Position 1 2

c3 :: Position
c3 = Position 2 2

c4 :: Position
c4 = Position 3 2

c5 :: Position
c5 = Position 4 2

c6 :: Position
c6 = Position 5 2

c7 :: Position
c7 = Position 6 2

c8 :: Position
c8 = Position 7 2


d1 :: Position
d1 = Position 0 3

d2 :: Position
d2 = Position 1 3

d3 :: Position
d3 = Position 2 3

d4 :: Position
d4 = Position 3 3

d5 :: Position
d5 = Position 4 3

d6 :: Position
d6 = Position 5 3

d7 :: Position
d7 = Position 6 3

d8 :: Position
d8 = Position 7 3


e1 :: Position
e1 = Position 0 4

e2 :: Position
e2 = Position 1 4

e3 :: Position
e3 = Position 2 4

e4 :: Position
e4 = Position 3 4

e5 :: Position
e5 = Position 4 4

e6 :: Position
e6 = Position 5 4

e7 :: Position
e7 = Position 6 4

e8 :: Position
e8 = Position 7 4


f1 :: Position
f1 = Position 0 5

f2 :: Position
f2 = Position 1 5

f3 :: Position
f3 = Position 2 5

f4 :: Position
f4 = Position 3 5

f5 :: Position
f5 = Position 4 5

f6 :: Position
f6 = Position 5 5

f7 :: Position
f7 = Position 6 5

f8 :: Position
f8 = Position 7 5


g1 :: Position
g1 = Position 0 6

g2 :: Position
g2 = Position 1 6

g3 :: Position
g3 = Position 2 6

g4 :: Position
g4 = Position 3 6

g5 :: Position
g5 = Position 4 6

g6 :: Position
g6 = Position 5 6

g7 :: Position
g7 = Position 6 6

g8 :: Position
g8 = Position 7 6


h1 :: Position
h1 = Position 0 7

h2 :: Position
h2 = Position 1 7

h3 :: Position
h3 = Position 2 7

h4 :: Position
h4 = Position 3 7

h5 :: Position
h5 = Position 4 7

h6 :: Position
h6 = Position 5 7

h7 :: Position
h7 = Position 6 7

h8 :: Position
h8 = Position 7 7

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

legalMonarch :: Position -> Board -> [Position]
legalMonarch position (Board board) =
  diagonals position ++ crosses position

legalRook :: Position -> Board -> [Position]
legalRook position board =
  foldMap (\direction -> goMany direction position [] board) [N, E, S, W]

goMany :: Direction -> Position -> [Position] -> Board -> [Position]
goMany direction position line board =
  case goOne direction position board of
    Nothing -> line
    Just next -> goMany direction next (line ++ [next]) board

goOne :: Direction -> Position -> Board -> Maybe Position
goOne direction (Position row column) board =
  case direction of
    N -> if row + 1 > 7 then Nothing else Just $ Position (row + 1) column
    NE -> if row + 1 > 7 || column + 1 > 7 then Nothing else Just $ Position (row + 1) (column + 1)
    E -> if column + 1 > 7 then Nothing else Just $ Position row (column + 1)
    SE -> if row - 1 < 0 || column + 1 > 7 then Nothing else Just $ Position (row - 1) (column + 1)
    S -> if row - 1 < 0 then Nothing else Just $ Position (row - 1) column
    SW -> if row - 1 < 0 || column - 1 < 0 then Nothing else Just $ Position (row - 1) (column - 1)
    W -> if column - 1 < 0 then Nothing else Just $ Position row (column - 1)
    NW -> if row + 1 > 7 || column - 1 < 0 then Nothing else Just $ Position (row + 1) (column - 1)

diagonals :: Position -> [Position]
diagonals (Position row column) =
  catMaybes [bottomLeft, bottomRight, topLeft, topRight]
    where
      bottomLeft = if row - 1 < 0 || column - 1 < 0 then Nothing else Just $ Position (row - 1) (column - 1)
      bottomRight = if row - 1 < 0 || column + 1 > 7 then Nothing else Just $ Position (row - 1) (column + 1)
      topLeft = if row + 1 > 7 || column - 1 < 0 then Nothing else Just $ Position (row + 1) (column - 1)
      topRight = if row + 1 > 7 || column + 1 > 7 then Nothing else Just $ Position (row + 1) (column + 1)

crosses :: Position -> [Position]
crosses (Position row column) =
  catMaybes [down, left, right, up]
    where
      down = if row - 1 < 0 then Nothing else Just $ Position (row - 1) column
      left = if column - 1 < 0 then Nothing else Just $ Position row (column - 1)
      right = if column + 1 > 7 then Nothing else Just $ Position row (column + 1)
      up = if row + 1 > 7 then Nothing else Just $ Position (row + 1) column
