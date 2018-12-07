module ChessSpec where

import Test.Hspec
import Data.Either

import Chess (place, emptyBoard, whiteKnight, remove, a1, h8, examine)

spec :: Spec
spec = do
  describe "Chess.place" $ do
    it "returns a new board with the piece placed on it" $ do
      Chess.place whiteKnight a1 emptyBoard `shouldNotBe` emptyBoard

  describe "Chess.remove" $ do
    it "returns a new board with the square cleared" $ do
      (Chess.remove a1 . Chess.place whiteKnight a1) emptyBoard `shouldBe` emptyBoard

  describe "Chess.examine" $ do
    it "returns the piece from a square if occupied" $ do
      (Chess.examine a1 . Chess.place whiteKnight a1) emptyBoard `shouldBe` (Just whiteKnight)
    it "returns nothing if empty" $ do
      Chess.examine a1 emptyBoard `shouldBe` Nothing

  describe "Chess.move" $ do
     it "no longer has piece in previous square" $ do
       (Chess.examine a1 . Chess.move a1 h8 . Chess.place whiteKnight a1) emptyBoard `shouldBe` Nothing
     it "has piece in new square" $ do
       (Chess.examine h8 . Chess.move a1 h8 . Chess.place whiteKnight a1) emptyBoard `shouldBe` (Just whiteKnight)

