module ChessSpec where

import Test.Hspec
import Data.Either
import Data.Maybe
import Test.QuickCheck

import Chess (place, emptyBoard, whiteKnight, remove, Position, makePosition, validIndex)

genValidPair :: Gen (Int, Int)
genValidPair = do
  rank <- choose (0, 7)
  file <- choose (0, 7)
  return (rank, file)

validIndexGen = choose (0, 7)

spec :: Spec
spec = do
  -- describe "Chess.place" $ do
  --   it "returns a new board with the piece placed on it" $ do
  --     either fail id $ do
  --       a1 <- validPosition 0 0
  --       return $ Chess.place whiteKnight a1 emptyBoard `shouldNotBe` emptyBoard

  -- describe "Chess.remove" $ do
  --   it "returns a new board with the square cleared" $ do
  --     either fail id $ do
  --       a1 <- validPosition 0 0
  --       return $ (Chess.remove a1 . Chess.place whiteKnight a1) emptyBoard `shouldBe` emptyBoard
  describe "Chess.makePosition" $
    it "considers all valid index as valid" $
      forAll validIndexGen $ \i -> validIndex i == True
  describe "Chess.makePosition" $
    it "returns valid positions if passed a valid pair" $
      forAll validIndexGen $ \rank ->
        forAll validIndexGen $ \file ->
          isJust $ makePosition rank file