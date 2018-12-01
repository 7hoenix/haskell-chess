module ChessSpec where

import Test.Hspec

import Chess (place, emptyBoard, whiteKnight, validPosition, remove)

spec :: Spec
spec = do
  describe "Chess.place" $ do
    it "returns a new board with the piece placed on it" $ do
      case validPosition 0 0 of
        Left _ -> 2 `shouldBe` 1
        Right a1 -> Chess.place whiteKnight a1 emptyBoard `shouldNotBe` emptyBoard

  describe "Chess.remove" $ do
    it "returns a new board with the square cleared" $ do
      case validPosition 0 0 of
        Left _ -> 2 `shouldBe` 1
        Right a1 -> (Chess.remove a1 . Chess.place whiteKnight a1) emptyBoard `shouldBe` emptyBoard
