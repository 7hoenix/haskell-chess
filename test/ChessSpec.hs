module ChessSpec where

import Test.Hspec
import Data.Either

import Chess (legal, place, emptyBoard, remove, get, move
             , knight, monarch
             , Team(..)
             , a1, a2, a3, a4, a5, a6, a7, a8
             , b1, b2, b3, b4, b5, b6, b7, b8
             , c1, c2, c3, c4, c5, c6, c7, c8
             , d1, d2, d3, d4, d5, d6, d7, d8
             , e1, e2, e3, e4, e5, e6, e7, e8
             , f1, f2, f3, f4, f5, f6, f7, f8
             , g1, g2, g3, g4, g5, g6, g7, g8
             , h1, h2, h3, h4, h5, h6, h7, h8
             )

spec :: Spec
spec = do
  describe "Chess.place" $ do
    it "returns a new board with the piece placed on it" $ do
      Chess.place (knight White) a1 emptyBoard `shouldNotBe` emptyBoard

  describe "Chess.remove" $ do
    it "returns a new board with the square cleared" $ do
      (Chess.remove a1 . Chess.place (knight White) a1) emptyBoard `shouldBe` emptyBoard

  describe "Chess.get" $ do
    it "returns the piece from a square if occupied" $ do
                                           (Chess.get a1 . Chess.place (knight White) a1) emptyBoard `shouldBe` (Just (knight White))
    it "returns nothing if empty" $ do
      Chess.get a1 emptyBoard `shouldBe` Nothing

  describe "Chess.move" $ do
     it "no longer has piece in previous square" $ do
       (Chess.get a1 . Chess.move a1 h8 . Chess.place (knight White) a1) emptyBoard `shouldBe` Nothing
     it "has piece in new square" $ do
       (Chess.get h8 . Chess.move a1 h8 . Chess.place (knight White) a1) emptyBoard `shouldBe` (Just (knight White))

  describe "Chess.legal" $ do
    it "knows monarch movement - bottom left" $ do
      (Chess.legal a1 . Chess.place (monarch White) a1) emptyBoard `shouldBe` [b2, b1, a2]
    it "knows monarch movement - top right" $ do
      (Chess.legal h8 . Chess.place (monarch White) h8) emptyBoard `shouldBe` [g7, h7, g8]
    -- it "knows monarch movement - center" $ do
    --   (Chess.legal b2 . Chess.place (monarch White) b2) emptyBoard `shouldBe` [a1, a2, a3, b1, b3, c1, c2, c3]


-- Add support for checks
