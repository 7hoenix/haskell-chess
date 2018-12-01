module ChessSpec where

import Test.Hspec
import Data.Either

import Chess (place, emptyBoard, whiteKnight, validPosition, remove)

spec :: Spec
spec = do
  describe "Chess.place" $ do
    it "returns a new board with the piece placed on it" $ do
      either fail id $ do
        a1 <- validPosition 0 0
        return $ Chess.place whiteKnight a1 emptyBoard `shouldNotBe` emptyBoard

  describe "Chess.remove" $ do
    it "returns a new board with the square cleared" $ do
      either fail id $ do
        a1 <- validPosition 0 0
        return $ (Chess.remove a1 . Chess.place whiteKnight a1) emptyBoard `shouldBe` emptyBoard
