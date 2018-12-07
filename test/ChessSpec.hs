module ChessSpec where

import Test.Hspec
import Data.Either

import Chess (place, emptyBoard, whiteKnight, remove, a1)

spec :: Spec
spec = do
  describe "Chess.place" $ do
    it "returns a new board with the piece placed on it" $ do
      Chess.place whiteKnight a1 emptyBoard `shouldNotBe` emptyBoard

  describe "Chess.remove" $ do
    it "returns a new board with the square cleared" $ do
      (Chess.remove a1 . Chess.place whiteKnight a1) emptyBoard `shouldBe` emptyBoard
